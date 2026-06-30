const http = require('http');
const { TextDecoder } = require('util');

const DEFAULT_RPARAMS = [
  'authyn',
  'trno',
  'trddt',
  'trdtm',
  'amt',
  'authno',
  'msg1',
  'msg2',
  'ordno',
  'isscd',
  'aqucd',
  'result',
  'resultcd'
];

const KSNET_ENDPOINTS = {
  pc: '/store/KSPayWebV1.4/web_host/recv_post.jsp',
  mobile: '/store/KSPayMobileV1.4/web_host/recv_post.jsp'
};

function readBody(req) {
  return new Promise((resolve, reject) => {
    const chunks = [];
    req.on('data', (chunk) => {
      chunks.push(Buffer.isBuffer(chunk) ? chunk : Buffer.from(chunk));
    });
    req.on('end', () => resolve(Buffer.concat(chunks).toString('utf8')));
    req.on('error', reject);
  });
}

function escapeHtml(value) {
  return String(value || '')
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;')
    .replace(/'/g, '&#39;');
}

function decodeKsnet(buffer) {
  try {
    return new TextDecoder('euc-kr').decode(buffer);
  } catch (error) {
    return buffer.toString('utf8');
  }
}

function parseQuery(req) {
  const url = new URL(req.url || '/', 'https://www.hueflowstudio.com');
  return Object.fromEntries(url.searchParams.entries());
}

function parseKsnetPayload(payload) {
  const raw = String(payload || '').replace(/\0/g, '').trim();
  const marker = raw.search(/[OX]`/);
  const normalized = marker >= 0 ? raw.slice(marker) : raw;
  const parts = normalized.split('`');

  if (parts.length < 3) {
    return null;
  }

  return DEFAULT_RPARAMS.reduce((acc, name, index) => {
    acc[name] = parts[index] || '';
    return acc;
  }, {});
}

function postToKsnet(path, fields) {
  const body = new URLSearchParams(fields).toString();

  return new Promise((resolve, reject) => {
    const req = http.request({
      host: 'kspay.ksnet.to',
      port: 80,
      path,
      method: 'POST',
      headers: {
        'Accept-Language': 'ko',
        'Content-Type': 'application/x-www-form-urlencoded',
        'Content-Length': Buffer.byteLength(body),
        'User-Agent': 'Mozilla/5.0 HueflowStudio-KSNET'
      },
      timeout: 15000
    }, (res) => {
      const chunks = [];
      res.on('data', (chunk) => chunks.push(Buffer.isBuffer(chunk) ? chunk : Buffer.from(chunk)));
      res.on('end', () => resolve(decodeKsnet(Buffer.concat(chunks))));
    });

    req.on('timeout', () => {
      req.destroy(new Error('KSNET request timed out'));
    });
    req.on('error', reject);
    req.write(body);
    req.end();
  });
}

async function fetchApproval(data, requestedMode) {
  const payKey = data.reCommConId || data.recommconid || data.sndCommConId || '';
  if (!payKey) {
    return null;
  }

  const fields = {
    sndCommConId: payKey,
    sndActionType: '1',
    sndAmount: data.sndAmount || data.amt || '',
    sndRpyParams: DEFAULT_RPARAMS.join('`')
  };

  const modeOrder = requestedMode === 'pc'
    ? ['pc', 'mobile']
    : requestedMode === 'mobile'
      ? ['mobile', 'pc']
      : ['mobile', 'pc'];

  let lastRaw = '';
  for (const mode of modeOrder) {
    const raw = await postToKsnet(KSNET_ENDPOINTS[mode], fields);
    lastRaw = raw;
    const parsed = parseKsnetPayload(raw);
    if (parsed) {
      parsed._ksnetMode = mode;
      return parsed;
    }
  }

  return {
    authyn: '',
    trno: '',
    trddt: '',
    trdtm: '',
    amt: fields.sndAmount,
    authno: '',
    msg1: 'KSNET 승인조회 실패',
    msg2: lastRaw.slice(0, 120),
    ordno: data.sndOrdernumber || '',
    isscd: '',
    aqucd: '',
    result: '',
    resultcd: ''
  };
}

function normalizeValues(data, approval) {
  return {
    authyn: approval?.authyn || data.authyn || data.reAuthyn || '',
    trno: approval?.trno || data.trno || data.reTrno || '',
    trdt: approval?.trddt || data.trdt || data.reTrddt || '',
    trtm: approval?.trdtm || data.trtm || data.reTrdtm || '',
    authno: approval?.authno || data.authno || data.reAuthno || '',
    ordno: approval?.ordno || data.ordno || data.reOrdno || data.sndOrdernumber || '',
    msg1: approval?.msg1 || data.msg1 || data.reMsg1 || '',
    msg2: approval?.msg2 || data.msg2 || data.reMsg2 || '',
    amt: approval?.amt || data.amt || data.reAmt || data.sndAmount || '',
    result: approval?.result || data.result || '',
    resultcd: approval?.resultcd || data.resultcd || ''
  };
}

function buildResultUrl(values) {
  const params = new URLSearchParams(values);
  return `/ksnet-result.html?${params.toString()}`;
}

module.exports = async function handler(req, res) {
  try {
    const query = parseQuery(req);
    const rawBody = req.method === 'POST' ? await readBody(req) : '';
    const body = new URLSearchParams(req.method === 'POST' ? rawBody : '');
    const data = {
      ...query,
      ...Object.fromEntries(body.entries())
    };

    const approval = await fetchApproval(data, query.mode || data.mode);
    const values = normalizeValues(data, approval);
    const resultUrl = buildResultUrl(values);

    res.setHeader('Content-Type', 'text/html; charset=utf-8');
    res.statusCode = 200;
    res.end(`<!doctype html>
<html lang="ko">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>KSNET 결제 결과</title>
<style>
  body { margin: 0; padding: 32px; font-family: -apple-system, BlinkMacSystemFont, "Pretendard", sans-serif; color: #1f1f1f; background: #faf9f5; }
  main { max-width: 520px; margin: 0 auto; }
  h1 { margin: 0 0 16px; font-size: 24px; font-weight: 500; }
  p { color: #6b6862; line-height: 1.7; }
  dl { display: grid; grid-template-columns: 120px 1fr; gap: 8px 16px; padding-top: 16px; border-top: 1px solid #e5e1d8; }
  dt { color: #6b6862; }
  dd { margin: 0; word-break: break-all; }
  button, a { margin-top: 24px; border: 0; background: #1f1f1f; color: white; padding: 12px 18px; font: inherit; cursor: pointer; text-decoration: none; display: inline-flex; }
</style>
</head>
<body>
<main>
  <h1>결제 결과를 정리하고 있습니다.</h1>
  <p>잠시 후 결제 결과 화면으로 이동합니다.</p>
  <dl>
    <dt>승인여부</dt><dd>${escapeHtml(values.authyn)}</dd>
    <dt>거래번호</dt><dd>${escapeHtml(values.trno)}</dd>
    <dt>주문번호</dt><dd>${escapeHtml(values.ordno)}</dd>
    <dt>승인번호</dt><dd>${escapeHtml(values.authno)}</dd>
    <dt>금액</dt><dd>${escapeHtml(values.amt)}</dd>
    <dt>메시지</dt><dd>${escapeHtml([values.msg1, values.msg2].filter(Boolean).join(' / '))}</dd>
  </dl>
  <a href="${escapeHtml(resultUrl)}">결과 화면으로 이동</a>
</main>
<script>
  (function(){
    var values = ${JSON.stringify(values)};
    var resultUrl = ${JSON.stringify(resultUrl)};

    function deliver(target) {
      if (!target || target === window) return false;
      try {
        if (typeof target.paramSet === 'function') {
          target.paramSet(
            values.authyn, values.trno, values.trdt, values.trtm,
            values.authno, values.ordno, values.msg1, values.msg2, values.amt
          );
          return true;
        }
        target.postMessage({ type: 'ksnet-payment-result', values: values }, 'https://www.hueflowstudio.com');
      } catch (error) {
        return false;
      }
      return false;
    }

    if (!deliver(window.opener) && !deliver(window.parent)) {
      window.location.replace(resultUrl);
    }
  })();
</script>
</body>
</html>`);
  } catch (error) {
    res.setHeader('Content-Type', 'text/html; charset=utf-8');
    res.statusCode = 500;
    res.end(`<!doctype html><meta charset="utf-8"><title>KSNET 오류</title><p>KSNET 결제 결과 처리 중 오류가 발생했습니다.</p><pre>${escapeHtml(error.message)}</pre>`);
  }
};
