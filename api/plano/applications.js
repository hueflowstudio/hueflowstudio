const {
  getStatus,
  listPaid,
  reserveApplicant
} = require('../../lib/plano-store');

function sendJson(res, statusCode, data) {
  res.statusCode = statusCode;
  res.setHeader('Content-Type', 'application/json; charset=utf-8');
  res.setHeader('Cache-Control', 'no-store');
  res.end(JSON.stringify(data));
}

function parseUrl(req) {
  return new URL(req.url || '/', 'https://www.hueflowstudio.com');
}

function readBody(req) {
  return new Promise((resolve, reject) => {
    const chunks = [];
    req.on('data', (chunk) => chunks.push(Buffer.isBuffer(chunk) ? chunk : Buffer.from(chunk)));
    req.on('end', () => resolve(Buffer.concat(chunks).toString('utf8')));
    req.on('error', reject);
  });
}

async function readJson(req) {
  const raw = await readBody(req);
  if (!raw) return {};
  try {
    return JSON.parse(raw);
  } catch (error) {
    throw new Error('Invalid JSON body');
  }
}

module.exports = async function handler(req, res) {
  if (req.method === 'OPTIONS') {
    res.statusCode = 204;
    res.end();
    return;
  }

  try {
    const url = parseUrl(req);
    const action = url.searchParams.get('action') || '';

    if (req.method === 'GET' && action === 'status') {
      sendJson(res, 200, await getStatus());
      return;
    }

    if (req.method === 'GET' && action === 'paid') {
      const token = url.searchParams.get('token') || '';
      if (!process.env.PLANO_ADMIN_TOKEN || token !== process.env.PLANO_ADMIN_TOKEN) {
        sendJson(res, 403, { error: 'Forbidden' });
        return;
      }
      sendJson(res, 200, await listPaid());
      return;
    }

    if (req.method === 'POST') {
      const applicant = await readJson(req);
      const reservation = await reserveApplicant(applicant);
      sendJson(res, reservation.soldOut ? 409 : 200, reservation);
      return;
    }

    res.setHeader('Allow', 'GET, POST, OPTIONS');
    sendJson(res, 405, { error: 'Method Not Allowed' });
  } catch (error) {
    sendJson(res, 400, { error: error.message || 'Application request failed' });
  }
};
