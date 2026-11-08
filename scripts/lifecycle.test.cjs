/* Run with: nix-shell -p nodejs --run 'node --test scripts/lifecycle.test.cjs' */
const test = require('node:test');
const assert = require('node:assert/strict');
const fs = require('node:fs');
const path = require('node:path');
const source = path.resolve(__dirname, '../docs/assets/lifecycle.js');
const { events, snapshot, options, fixture, progress, remainder, groupAttributes, measurementState } = require(source);
const last = config => events(config).at(-1);
const total = config => fixture(config).response.body.bytes.length;

test('UTF-8 fixtures, headers, exact chunk slices and progressively decoded previews agree', () => {
  for (const method of ['GET', 'POST', 'HEAD']) {
    for (const status of [200, 204, 404, 503]) {
      const config = { method, status };
      const data = fixture(config);
      for (const direction of ['request', 'response']) {
        const message = data[direction];
        assert.deepEqual(message.body.chunks.flatMap(chunk => chunk.bytes), message.body.bytes);
        let end = 0;
        for (let count = 0; count <= message.body.chunks.length; count++) {
          const seen = progress(message.body, count);
          assert.deepEqual(seen.bytes, message.body.bytes.slice(0, end));
          const remaining = remainder(message.body, count);
          assert.deepEqual(remaining.bytes, message.body.bytes.slice(end));
          assert.equal(seen.text + remaining.text, new TextDecoder().decode(Uint8Array.from(message.body.bytes)));
          assert.equal(remaining.completesCharacter, seen.pendingBytes > 0);
          assert.equal(new TextEncoder().encode(seen.text).length + seen.pendingBytes, seen.bytes.length);
          const next = message.body.chunks[count];
          if (next) {
            assert.equal(next.start, end);
            assert.equal(next.end - next.start, next.bytes.length);
            end = next.end;
          }
        }
        assert.equal(progress(message.body, message.body.chunks.length).text,
          new TextDecoder().decode(Uint8Array.from(message.body.bytes)));
      }
      if (method === 'POST') {
        assert.equal(Number(data.request.headers.find(([key]) => key === 'Content-Length')[1]), data.request.body.bytes.length);
        assert.equal(progress(data.request.body, 1).pendingBytes, 1);
      }
      if (status === 204) {
        assert.equal(data.response.declaredLength, null);
        assert.equal(data.response.headers.some(([key]) => key === 'Content-Length'), false);
      } else {
        assert.equal(Number(data.response.headers.find(([key]) => key === 'Content-Length')[1]), data.response.declaredLength);
        if (method !== 'HEAD') { assert.equal(data.response.declaredLength, data.response.body.bytes.length); }
      }
      if (method === 'HEAD' || status === 204) { assert.equal(total(config), 0); }
    }
  }
  assert.equal(progress(fixture().response.body, 1).pendingBytes, 1);
});

test('body bytes advance only in explicit chunk evaluations; EOF is a separate event', () => {
  for (const method of ['GET', 'POST', 'HEAD']) {
    for (const scenario of ['success', 'body-error', 'body-cancel', 'unread', 'app-error', 'app-cancel']) {
      const config = { method, scenario };
      const data = fixture(config);
      let previous = { bytes: 0, requestBytes: 0 };
      for (const event of events(config)) {
        assert.equal(event.bytes, progress(data.response.body, event.chunkCount).bytes.length);
        assert.equal(event.requestBytes, progress(data.request.body, event.requestChunkCount).bytes.length);
        if (event.chunk) {
          const request = event.chunk.direction === 'request';
          const key = request ? 'requestBytes' : 'bytes';
          assert.equal(event[key] - previous[key], event.chunk.bytes.length);
          assert.deepEqual(event.chunk.bytes, data[event.chunk.direction].body.bytes.slice(event.chunk.start, event.chunk.end));
          assert.equal(event.bodySize, null);
        } else {
          assert.equal(event.bytes, previous.bytes);
          assert.equal(event.requestBytes, previous.requestBytes);
        }
        previous = event;
      }
      const final = last(config);
      if (scenario === 'success') {
        assert.equal(final.bytes, total(config));
        assert.equal(final.bodyComplete, true);
      } else if (scenario === 'body-error' || scenario === 'body-cancel') {
        if (total(config)) { assert.ok(final.bytes < total(config)); }
        assert.equal(final.bodyComplete, false);
      } else { assert.equal(final.bytes, 0); }
    }
  }
  const allBytes = events().find(event => event.chunkCount === 3);
  assert.equal(allBytes.bodyComplete, false);
  assert.equal(allBytes.bodySize, null);
});

test('span status is distinct from open/ended lifecycle and remains Unset until Error is set', () => {
  for (const config of [{}, { status: 503 }, { scenario: 'body-error' }, { scenario: 'body-cancel' }]) {
    for (const event of events(config)) {
      const data = snapshot(config, event);
      if (!data.traced) { assert.equal(data.spanStatus, null); }
      else if (event.span === 'open') { assert.equal(data.spanStatus, 'Unset'); }
      else { assert.equal(data.spanStatus, event.error ? 'Error' : 'Unset'); }
    }
  }
  assert.equal(snapshot({ tracing: false }, last({ tracing: false })).spanStatus, null);
});

test('a successful exchange records once, before span end and backend release', () => {
  const timeline = events();
  const recording = timeline.findIndex(event => event.phase === 'metrics');
  const metrics = timeline[recording];
  assert.equal(metrics.span, 'open');
  assert.equal(metrics.durationSeconds, 0.12);
  assert.equal(metrics.bodySize, total());
  assert.equal(timeline[recording + 1].span, 'ended');
  assert.equal(timeline[recording + 1].time, metrics.time);
  assert.equal(last().durationSeconds, metrics.durationSeconds);
  assert.equal(timeline.filter((event, index) => event.duration && !timeline[index - 1]?.duration).length, 1);
  const data = snapshot({}, metrics);
  assert.equal(data.count, 1);
  assert.equal(data.buckets.find(bucket => bucket.le === 0.1).count, 0);
  assert.equal(data.buckets.find(bucket => bucket.le === 0.25).count, 1);
  assert.equal(data.exemplarSpan, data.local);
});

test('unread, incomplete and protocol-known-zero sizes remain distinct', () => {
  const server = last({ side: 'server', scenario: 'unread' });
  assert.equal(server.span, 'open');
  assert.equal(server.active, 1);
  assert.equal(server.duration, false);
  assert.equal(server.bodySize, null);
  const client = last({ scenario: 'unread' });
  assert.equal(client.span, 'ended');
  assert.equal(client.active, 0);
  assert.equal(client.durationSeconds, 0.075);
  assert.equal(client.bodySize, null);
  assert.equal(last({ scenario: 'unread', bodySize: 'contentLength' }).bodySize, fixture().response.declaredLength);
  assert.equal(last({ side: 'server', scenario: 'unread', status: 204 }).bodySize, null);
  assert.equal(last({ status: 204 }).bodySize, 0);
  assert.equal(last({ method: 'HEAD' }).bodySize, 0);
  assert.ok(fixture({ method: 'HEAD' }).response.declaredLength > 0);
});

test('status policy, observed errors and cancellations remain distinct', () => {
  assert.equal(last({ status: 404 }).errorType, '404');
  assert.equal(last({ side: 'server', status: 404 }).errorType, null);
  assert.equal(last({ side: 'server', status: 503 }).errorType, '503');
  assert.equal(last({ scenario: 'body-error', status: 503 }).errorType, 'demo.BodyFailure');
  assert.equal(last({ scenario: 'body-cancel' }).error, false);
  assert.equal(last({ scenario: 'body-cancel', status: 503 }).errorType, '503');
  assert.equal(last({ scenario: 'app-cancel', status: 503 }).error, false);
  assert.equal(last({ scenario: 'body-error' }).bodySize, null);
  assert.equal(last({ scenario: 'body-cancel', bodySize: 'contentLength' }).bodySize, total());
});

test('messages, attributes, redaction, capture and parent/exemplar links have consistent sources', () => {
  const atHeaders = config => snapshot(config, events(config).find(event => event.phase === 'headers'));
  const client = atHeaders({});
  const server = atHeaders({ side: 'server', reverse: true });
  assert.ok(client.requestHeader.includes(client.local));
  assert.equal(server.parentId, client.local);
  assert.equal(server.requestHeader, client.requestHeader);
  assert.ok(server.responseHeader.includes(server.local));
  assert.equal(server.baggage, 'tenant=demo');
  const headers = Object.fromEntries(server.messages.response.headers);
  assert.equal(headers.traceparent, server.responseHeader);
  assert.equal(headers.baggage, server.baggage);
  assert.equal(atHeaders({ propagate: false }).requestHeader, null);
  const metricsOnlyClient = snapshot({ tracing: false }, last({ tracing: false }));
  assert.equal(metricsOnlyClient.requestHeader, null);
  assert.equal(metricsOnlyClient.count, 1);
  assert.equal(metricsOnlyClient.exemplarSpan, '1111111111111111');
  const metricsOnlyServer = atHeaders({ side: 'server', tracing: false, reverse: true });
  assert.ok(metricsOnlyServer.requestHeader); // The header exists but is not extracted.
  assert.equal(metricsOnlyServer.responseHeader, null);
  assert.equal(metricsOnlyServer.scope, null);
  assert.equal(metricsOnlyServer.baggage, 'none');
  const root = atHeaders({ side: 'server', parent: false });
  assert.equal(root.parentId, null);
  assert.notEqual(root.traceId, server.traceId);
  assert.equal(client.spanAttributes.find(attr => attr.key === 'url.full').value,
    'http://api.example.test/widgets/42?access_token=REDACTED');
  assert.ok(client.messages.request.query.includes('demo-secret'));
  assert.equal(client.spanAttributes.some(attr => attr.key === 'http.response.status_code'), false);
  const finished = snapshot({}, last());
  assert.equal(finished.spanAttributes.find(attr => attr.key === 'http.response.status_code').value, fixture().response.status);
  assert.equal(finished.spanAttributes.some(attr => attr.key.includes('header.')), false);
  const captured = snapshot({ captureHeaders: true, method: 'POST' }, last({ captureHeaders: true, method: 'POST' }));
  const length = captured.spanAttributes.find(attr => attr.key === 'http.request.header.content-length').value[0];
  assert.equal(Number(length), fixture({ method: 'POST' }).request.body.bytes.length);
  assert.equal(captured.spanAttributes.some(attr => attr.key.endsWith('.baggage') || attr.key.endsWith('.traceparent')), false);
  assert.equal(server.startAttributes.some(attr => attr.key === 'http.route'), false);
  assert.equal(snapshot({ side: 'server' }, last({ side: 'server' })).terminalAttributes.some(attr => attr.key === 'http.route'), true);
});

test('all scenario/policy combinations preserve recording, span and fixture invariants', () => {
  for (const side of ['client', 'server']) {
    for (const scenario of ['success', 'body-error', 'body-cancel', 'unread', 'app-error', 'app-cancel']) {
      for (const status of [200, 204, 404, 503]) {
        for (const method of ['GET', 'POST', 'HEAD']) {
          for (let bits = 0; bits < 64; bits++) {
            const config = options({ side, scenario, status, method, metrics: !!(bits & 1), tracing: !!(bits & 2),
              parent: !!(bits & 4), propagate: !!(bits & 8), reverse: !!(bits & 16), bodySize: bits & 32 ? 'observe' : 'contentLength' });
            const timeline = events(config);
            const final = timeline.at(-1);
            const incomplete = side === 'server' && scenario === 'unread';
            assert.equal(final.active, incomplete && config.metrics ? 1 : 0);
            assert.equal(final.duration, config.metrics && !incomplete);
            assert.equal(final.span, !config.tracing ? 'disabled' : incomplete ? 'open' : 'ended');
            let wasRecorded = false;
            for (const event of timeline) {
              const data = snapshot(config, event);
              assert.ok(event.bytes <= data.messages.response.body.bytes.length);
              assert.ok(event.requestBytes <= data.messages.request.body.bytes.length);
              assert.ok(event.active === 0 || event.active === 1);
              if (wasRecorded) { assert.equal(event.duration, true); }
              wasRecorded ||= event.duration;
              if (!config.metrics) {
                assert.equal(data.count, 0);
                assert.equal(data.sizeCount, 0);
                assert.equal(data.exemplarSpan, null);
              }
              if (!config.tracing) { assert.equal(data.traced, false); }
              if (event.bodySize !== null && config.bodySize === 'observe') {
                assert.equal(event.bodySize, event.bytes);
              }
            }
          }
        }
      }
    }
  }
});

test('shared attributes merge only when key, value and source agree', () => {
  const entry = { key: 'http.request.method', value: 'GET', source: 'method' };
  const grouped = groupAttributes([
    { ...entry, destinations: ['Span'] },
    { ...entry, destinations: ['Active requests'] },
    { ...entry, destinations: ['Duration', 'Body size', 'Span'] },
    { ...entry, value: 'POST', destinations: ['Span'] },
    { ...entry, source: 'other', destinations: ['Span'] }
  ]);
  assert.equal(grouped.length, 3);
  assert.deepEqual(grouped[0].destinations, ['Span', 'Active requests', 'Duration', 'Body size']);
});

test('missing measurements explain the actual lifecycle state', () => {
  const state = (config, event, metric) => measurementState(snapshot(config, event), event, metric);
  assert.equal(state({}, events()[0], 'duration'), 'Not started');
  assert.equal(state({ metrics: false }, last({ metrics: false }), 'size'), 'Disabled by metrics filter');
  for (const scenario of ['app-error', 'app-cancel']) {
    assert.equal(state({ scenario }, last({ scenario }), 'size'), 'Not recorded: no response');
    assert.equal(state({ scenario }, last({ scenario }), 'duration'), 'Recorded');
  }
  assert.equal(state({ side: 'server', scenario: 'unread' }, last({ side: 'server', scenario: 'unread' }), 'duration'), 'Not recorded: body never started');
  assert.equal(state({ scenario: 'unread' }, last({ scenario: 'unread' }), 'size'), 'Not recorded: body was unread');
  assert.equal(state({ scenario: 'body-error' }, last({ scenario: 'body-error' }), 'size'), 'Not recorded: body failed before EOF');
  assert.equal(state({ scenario: 'body-cancel' }, last({ scenario: 'body-cancel' }), 'size'), 'Not recorded: body was canceled before EOF');
  assert.equal(state({ bodySize: 'contentLength', scenario: 'unread' }, last({ bodySize: 'contentLength', scenario: 'unread' }), 'size'), 'Recorded');
});

test('inspector selection, linked fields, exemplar navigation and all scenario controls work', () => {
  const markdown = fs.readFileSync(path.resolve(__dirname, '../docs/lifecycle.md'), 'utf8');
  const ids = [...markdown.matchAll(/id="([^"]+)"/g)].map(match => match[1]);
  assert.equal(ids.length, new Set(ids).size);
  const elements = new Map();
  function element() {
    return { children: [], style: {}, dataset: {}, handlers: {},
      appendChild(child) {
        if (child.parentNode) {
          child.parentNode.children = child.parentNode.children.filter(node => node !== child);
        }
        child.parentNode = this;
        this.children.push(child);
      },
      replaceChildren(...children) {
        this.children.forEach(child => { child.parentNode = null; });
        this.children = [];
        children.forEach(child => this.appendChild(child));
      },
      setAttribute(key, value) { this[key] = value; }, addEventListener(name, handler) { this.handlers[name] = handler; }
    };
  }
  ids.forEach(id => elements.set(id, element()));
  const get = name => elements.get('lifecycle-' + name);
  for (const [name, value] of Object.entries(options())) {
    Object.assign(get(name), typeof value === 'boolean' ? { type: 'checkbox', checked: value } : { value: String(value) });
  }
  function all(root) { return [root, ...root.children.flatMap(all)]; }
  function text(root) { return [root.textContent || '', ...root.children.map(text)].join(''); }
  global.document = { readyState: 'complete', getElementById: id => elements.get(id), createElement: tag => Object.assign(element(), { tagName: tag.toUpperCase() }),
    createElementNS: (_, tag) => Object.assign(element(), { tagName: tag.toUpperCase() }) };
  delete require.cache[require.resolve(source)];
  require(source);
  assert.equal(get('widget').hidden, false);
  assert.equal(get('inspector').hidden, true);
  get('request').handlers.click();
  assert.equal(get('inspector').hidden, false);
  assert.match(get('inspector-title').textContent, /HTTP messages/);
  assert.match(text(get('inspector-content')), /Prepared request/);
  assert.equal(get('field-note').hidden, true);
  assert.doesNotMatch(text(get('inspector-content')), /Full fixture|0\/0 B/);
  get('next').handlers.click();
  get('trace').children.at(-1).handlers.click();
  assert.match(get('inspector-title').textContent, /GET \/widgets/);
  assert.equal(get('inspector').parentNode, get('trace'));
  assert.equal(get('trace').children.at(-1), get('inspector'));
  const expandedSpan = get('trace').children.find(node => node['aria-expanded'] === 'true');
  assert.ok(expandedSpan);
  expandedSpan.handlers.click();
  assert.equal(get('inspector').hidden, true);
  get('trace').children.at(-1).handlers.click();
  const method = all(get('inspector-content')).find(node => node.textContent === 'GET ');
  method.handlers.click();
  assert.ok(all(get('inspector-content')).filter(node => node.dataset.highlighted === 'true').length >= 2);
  assert.match(get('field-note').textContent, /http.request.method/);
  for (const side of ['client', 'server']) {
    get('side').value = side; get('side').handlers.change();
    assert.equal(get('reverse').disabled, side !== 'server');
    assert.equal(get('propagate').disabled, side !== 'client');
    for (const scenario of ['success', 'unread', 'body-error', 'body-cancel', 'app-error', 'app-cancel']) {
      for (const method of ['GET', 'POST', 'HEAD']) {
        get('method').value = method; get('method').handlers.change();
        get('scenario').value = scenario; get('scenario').handlers.change();
        assert.equal(get('back').disabled, true);
        get('request').handlers.click();
        let clicks = 0;
        while (!get('next').disabled) {
          get('next').handlers.click();
          assert.ok(++clicks < 20);
          const current = events({ side, scenario, method })[clicks];
          if (current.chunk) {
            const payload = fixture({ method })[current.chunk.direction].body;
            const count = current.chunk.direction === 'request' ? current.requestChunkCount : current.chunkCount;
            const preview = all(get('inspector-content')).find(node => node.className === 'lc-body-preview');
            assert.equal(text(preview), progress(payload, count).text);
            const remaining = all(get('inspector-content')).find(node => node.textContent?.startsWith('Remaining example body'));
            assert.equal(!!remaining, progress(payload, count).bytes.length < payload.bytes.length);
            const chunkDetails = all(get('inspector-content')).find(node => node.tagName === 'DETAILS' && text(node).includes('exact bytes'));
            assert.ok(chunkDetails);
            assert.ok(!chunkDetails.open);
            assert.equal(preview.role, 'button');
            assert.equal(preview.tabindex, '0');
            assert.ok(preview.children.every(segment => segment.tagName === 'SPAN'));
            let prevented = 0;
            preview.handlers.keydown({ key: 'Enter', preventDefault() { prevented++; } });
            preview.handlers.keydown({ key: ' ', preventDefault() { prevented++; } });
            assert.equal(prevented, 2);
            assert.equal(preview['aria-pressed'], 'true');
            assert.ok(text(get('inspector-content')).includes('[' + current.chunk.start + ', ' + current.chunk.end + ')'));
          }
        }
        assert.equal(get('steps').children.filter(button => button['aria-current'] === 'step').length, 1);
        get('request').handlers.click();
        const methodRows = all(get('inspector-content')).filter(node => node.className === 'lc-detail-row lc-linked' && node.children[0]?.textContent === 'http.request.method');
        assert.equal(methodRows.length, 1);
        assert.match(text(methodRows[0]), /Span.*Active requests/);
        if (scenario !== 'unread' || side === 'client') {
          assert.match(text(methodRows[0]), /Duration/);
          assert.equal(text(methodRows[0]).includes('Body size'), last({ side, scenario, method }).bodySize !== null);
        }
        const metricRows = () => get('metrics-overview').children.filter(node => node.className?.includes('lc-metric-row'));
        assert.equal(metricRows().length, 3);
        for (let index = 0; index < 3; index++) {
          metricRows()[index].children[0].handlers.click();
          const selected = metricRows()[index];
          assert.equal(selected.children[0]['aria-expanded'], 'true');
          assert.equal(get('inspector').hidden, false);
          assert.equal(get('inspector').parentNode, get('metrics-overview'));
          const siblings = get('metrics-overview').children;
          assert.equal(siblings[siblings.indexOf(selected) + 1], get('inspector'));
          assert.ok(all(get('inspector-content')).some(node => node.className === 'lc-message-separator'));
          metricRows()[index].children[0].handlers.click();
          assert.equal(get('inspector').hidden, true);
          assert.equal(get('inspector').parentNode, get('inspector-home'));
        }
        metricRows()[1].children[1].handlers.click();
        assert.match(get('inspector-title').textContent, /request.duration/);
        const exemplar = all(get('inspector-content')).find(node => node.className === 'lc-exemplar');
        if (exemplar) {
          exemplar.handlers.click();
          assert.match(get('inspector-title').textContent, /widgets/);
          assert.equal(get('inspector').parentNode, get('trace'));
          assert.equal(get('trace').children.at(-1), get('inspector'));
        }
        get('back').handlers.click();
        assert.equal(get('next').disabled, false);
        get('steps').children[0].handlers.click();
        assert.equal(get('back').disabled, true);
        get('inspector-close').handlers.click();
        assert.equal(get('inspector').hidden, true);
        get('reset').handlers.click();
      }
    }
  }
  get('tracing').checked = false; get('tracing').handlers.change();
  get('metrics').checked = false; get('metrics').handlers.change();
  assert.match(text(get('metrics-overview')), /disabled/);
  delete global.document;
});
