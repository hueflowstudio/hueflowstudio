const COURSE_ID = process.env.PLANO_COURSE_ID || 'plano-02-online-intensive';
const COURSE_KEY = `hf:${COURSE_ID}`;
const DEFAULT_CAPACITY = 30;
const DEFAULT_TTL_SECONDS = 60 * 60;

const keys = {
  paidSet: `${COURSE_KEY}:paid-orders`,
  pendingSet: `${COURSE_KEY}:pending-orders`,
  pending(orderNumber) {
    return `${COURSE_KEY}:pending:${orderNumber}`;
  },
  paid(orderNumber) {
    return `${COURSE_KEY}:paid:${orderNumber}`;
  }
};

function getCapacity() {
  const capacity = Number.parseInt(process.env.PLANO_CAPACITY || '', 10);
  return Number.isFinite(capacity) && capacity > 0 ? capacity : DEFAULT_CAPACITY;
}

function getReservationTtl() {
  const ttl = Number.parseInt(process.env.PLANO_RESERVATION_TTL_SECONDS || '', 10);
  return Number.isFinite(ttl) && ttl > 120 ? ttl : DEFAULT_TTL_SECONDS;
}

function getRedisConfig() {
  const url = process.env.KV_REST_API_URL || process.env.UPSTASH_REDIS_REST_URL || '';
  const token = process.env.KV_REST_API_TOKEN || process.env.UPSTASH_REDIS_REST_TOKEN || '';
  return {
    configured: Boolean(url && token),
    url: url.replace(/\/$/, ''),
    token
  };
}

function isConfigured() {
  return getRedisConfig().configured;
}

async function redis(command) {
  const config = getRedisConfig();
  if (!config.configured) {
    throw new Error('Plano storage is not configured');
  }

  const res = await fetch(config.url, {
    method: 'POST',
    headers: {
      Authorization: `Bearer ${config.token}`,
      'Content-Type': 'application/json'
    },
    body: JSON.stringify(command)
  });
  const data = await res.json().catch(() => ({}));
  if (!res.ok || data.error) {
    throw new Error(data.error || `Redis command failed: ${command[0]}`);
  }
  return data.result;
}

function parseStoredJson(value) {
  if (!value) return null;
  if (typeof value === 'object') return value;
  try {
    return JSON.parse(value);
  } catch (error) {
    return null;
  }
}

function sanitizeApplicant(applicant) {
  const phone = String(applicant.phone || '').replace(/[^\d]/g, '');
  return {
    course: COURSE_ID,
    orderNumber: String(applicant.orderNumber || '').trim(),
    name: String(applicant.name || '').trim(),
    phone,
    email: String(applicant.email || '').trim(),
    social: String(applicant.social || '').trim(),
    amount: Number(applicant.amount || 590000),
    source: String(applicant.source || 'Plano.html').slice(0, 80),
    createdAt: applicant.createdAt || new Date().toISOString()
  };
}

function assertApplicant(applicant) {
  if (!applicant.orderNumber) throw new Error('orderNumber is required');
  if (!applicant.name || applicant.name.length < 2) throw new Error('name is required');
  if (!applicant.phone || applicant.phone.length < 9) throw new Error('phone is required');
}

async function cleanupExpiredPending() {
  const pendingOrders = await redis(['SMEMBERS', keys.pendingSet]);
  if (!Array.isArray(pendingOrders) || pendingOrders.length === 0) return 0;

  let removed = 0;
  await Promise.all(pendingOrders.map(async (orderNumber) => {
    const pending = await redis(['GET', keys.pending(orderNumber)]);
    if (!pending) {
      await redis(['SREM', keys.pendingSet, orderNumber]);
      removed += 1;
    }
  }));
  return removed;
}

async function getStatus() {
  const capacity = getCapacity();
  if (!isConfigured()) {
    return {
      configured: false,
      capacity,
      paidCount: null,
      pendingCount: null,
      remaining: null,
      soldOut: false
    };
  }

  await cleanupExpiredPending();
  const [paidRaw, pendingRaw] = await Promise.all([
    redis(['SCARD', keys.paidSet]),
    redis(['SCARD', keys.pendingSet])
  ]);
  const paidCount = Number(paidRaw || 0);
  const pendingCount = Number(pendingRaw || 0);
  const remaining = Math.max(0, capacity - paidCount - pendingCount);
  return {
    configured: true,
    capacity,
    paidCount,
    pendingCount,
    remaining,
    soldOut: remaining <= 0
  };
}

async function reserveApplicant(rawApplicant) {
  const capacity = getCapacity();
  const applicant = sanitizeApplicant(rawApplicant);
  assertApplicant(applicant);

  if (!isConfigured()) {
    return {
      configured: false,
      capacity,
      orderNumber: applicant.orderNumber,
      soldOut: false
    };
  }

  await cleanupExpiredPending();
  const reserveScript = `
    local paid = redis.call("SCARD", KEYS[1])
    local pending = redis.call("SCARD", KEYS[2])
    if (paid + pending) >= tonumber(ARGV[2]) then
      return 0
    end
    redis.call("SADD", KEYS[2], ARGV[1])
    return 1
  `;
  const reserved = await redis([
    'EVAL',
    reserveScript,
    2,
    keys.paidSet,
    keys.pendingSet,
    applicant.orderNumber,
    String(capacity)
  ]);

  if (Number(reserved) !== 1) {
    const status = await getStatus();
    return {
      ...status,
      orderNumber: applicant.orderNumber,
      soldOut: true
    };
  }

  await redis([
    'SET',
    keys.pending(applicant.orderNumber),
    JSON.stringify(applicant),
    'EX',
    String(getReservationTtl())
  ]);
  const status = await getStatus();
  return {
    ...status,
    orderNumber: applicant.orderNumber,
    reserved: true
  };
}

async function recordPayment(orderNumber, values = {}) {
  const cleanOrderNumber = String(orderNumber || '').trim();
  if (!cleanOrderNumber || !isConfigured()) {
    return { configured: isConfigured(), recorded: false };
  }

  const pending = parseStoredJson(await redis(['GET', keys.pending(cleanOrderNumber)]));
  const record = {
    course: COURSE_ID,
    orderNumber: cleanOrderNumber,
    applicant: pending,
    payment: {
      authyn: values.authyn || '',
      trno: values.trno || '',
      trdt: values.trdt || '',
      trtm: values.trtm || '',
      authno: values.authno || '',
      amt: values.amt || '',
      msg1: values.msg1 || '',
      msg2: values.msg2 || '',
      result: values.result || '',
      resultcd: values.resultcd || ''
    },
    paidAt: new Date().toISOString()
  };

  await Promise.all([
    redis(['SET', keys.paid(cleanOrderNumber), JSON.stringify(record)]),
    redis(['SADD', keys.paidSet, cleanOrderNumber]),
    redis(['SREM', keys.pendingSet, cleanOrderNumber]),
    redis(['DEL', keys.pending(cleanOrderNumber)])
  ]);
  return { configured: true, recorded: true, orderNumber: cleanOrderNumber };
}

async function listPaid() {
  if (!isConfigured()) {
    return { configured: false, records: [] };
  }

  const orderNumbers = await redis(['SMEMBERS', keys.paidSet]);
  const records = await Promise.all((orderNumbers || []).map(async (orderNumber) => {
    return parseStoredJson(await redis(['GET', keys.paid(orderNumber)]));
  }));

  return {
    configured: true,
    records: records.filter(Boolean).sort((a, b) => String(a.paidAt).localeCompare(String(b.paidAt)))
  };
}

module.exports = {
  getStatus,
  isConfigured,
  listPaid,
  recordPayment,
  reserveApplicant
};
