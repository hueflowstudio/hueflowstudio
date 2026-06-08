function readBody(req) {
  return new Promise((resolve, reject) => {
    let body = '';
    req.on('data', (chunk) => {
      body += chunk.toString('utf8');
    });
    req.on('end', () => resolve(body));
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

module.exports = async function handler(req, res) {
  const rawBody = req.method === 'POST' ? await readBody(req) : '';
  const params = new URLSearchParams(req.method === 'POST' ? rawBody : req.url.split('?')[1] || '');
  const data = Object.fromEntries(params.entries());

  const values = {
    authyn: data.authyn || data.reAuthyn || '',
    trno: data.trno || data.reTrno || '',
    trdt: data.trdt || data.reTrddt || '',
    trtm: data.trtm || data.reTrdtm || '',
    authno: data.authno || data.reAuthno || '',
    ordno: data.ordno || data.reOrdno || data.sndOrdernumber || '',
    msg1: data.msg1 || data.reMsg1 || '',
    msg2: data.msg2 || data.reMsg2 || '',
    amt: data.amt || data.reAmt || data.sndAmount || ''
  };

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
  button { margin-top: 24px; border: 0; background: #1f1f1f; color: white; padding: 12px 18px; font: inherit; cursor: pointer; }
</style>
</head>
<body>
<main>
  <h1>결제 결과를 확인했습니다.</h1>
  <p>이 창은 KSNET 인증결제 결과 수신용입니다. 결과가 부모 창으로 전달되면 자동으로 정리됩니다.</p>
  <dl>
    <dt>승인여부</dt><dd>${escapeHtml(values.authyn)}</dd>
    <dt>거래번호</dt><dd>${escapeHtml(values.trno)}</dd>
    <dt>주문번호</dt><dd>${escapeHtml(values.ordno)}</dd>
    <dt>승인번호</dt><dd>${escapeHtml(values.authno)}</dd>
    <dt>금액</dt><dd>${escapeHtml(values.amt)}</dd>
    <dt>메시지</dt><dd>${escapeHtml([values.msg1, values.msg2].filter(Boolean).join(' / '))}</dd>
  </dl>
  <button type="button" onclick="window.close()">닫기</button>
</main>
<script>
  (function(){
    var values = ${JSON.stringify(values)};
    if (window.opener && !window.opener.closed) {
      if (typeof window.opener.paramSet === 'function') {
        window.opener.paramSet(
          values.authyn, values.trno, values.trdt, values.trtm,
          values.authno, values.ordno, values.msg1, values.msg2, values.amt
        );
      } else {
        window.opener.postMessage({ type: 'ksnet-payment-result', values: values }, 'https://www.hueflowstudio.com');
      }
    }
  })();
</script>
</body>
</html>`);
};
