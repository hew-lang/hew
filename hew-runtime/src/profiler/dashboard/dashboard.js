// Hew Profiler Dashboard — polling + canvas charting (no dependencies).
"use strict";

const POLL_MS = 1000;
let prevSnap = null;

// ── Formatting ──────────────────────────────────────────────────────────

function fmtBytes(b) {
  if (b < 1024) return b + " B";
  if (b < 1024 * 1024) return (b / 1024).toFixed(1) + " KB";
  if (b < 1024 * 1024 * 1024) return (b / (1024 * 1024)).toFixed(1) + " MB";
  return (b / (1024 * 1024 * 1024)).toFixed(2) + " GB";
}

function fmtNum(n) {
  if (n < 1000) return String(n);
  if (n < 1_000_000) return (n / 1000).toFixed(1) + "K";
  return (n / 1_000_000).toFixed(1) + "M";
}

function fmtRate(n) { return fmtNum(Math.round(n)); }

function fmtUptime(secs) {
  const h = Math.floor(secs / 3600);
  const m = Math.floor((secs % 3600) / 60);
  const s = secs % 60;
  if (h > 0) return `${h}h ${m}m ${s}s`;
  if (m > 0) return `${m}m ${s}s`;
  return `${s}s`;
}

// ── Sparkline renderer ──────────────────────────────────────────────────

function drawSparkline(canvasId, series, opts = {}) {
  const canvas = document.getElementById(canvasId);
  if (!canvas) return;
  const ctx = canvas.getContext("2d");
  const dpr = window.devicePixelRatio || 1;
  const rect = canvas.getBoundingClientRect();
  canvas.width = rect.width * dpr;
  canvas.height = rect.height * dpr;
  ctx.scale(dpr, dpr);
  const W = rect.width, H = rect.height;

  ctx.clearRect(0, 0, W, H);

  for (const { data, color, fill } of series) {
    if (!data || data.length < 2) continue;
    const max = Math.max(...data, 1);
    const step = W / (data.length - 1);

    ctx.beginPath();
    for (let i = 0; i < data.length; i++) {
      const x = i * step;
      const y = H - (data[i] / max) * (H - 4) - 2;
      i === 0 ? ctx.moveTo(x, y) : ctx.lineTo(x, y);
    }
    ctx.strokeStyle = color;
    ctx.lineWidth = 1.5;
    ctx.stroke();

    if (fill) {
      ctx.lineTo((data.length - 1) * step, H);
      ctx.lineTo(0, H);
      ctx.closePath();
      ctx.fillStyle = color.replace(")", ", 0.1)").replace("rgb", "rgba");
      ctx.fill();
    }
  }

  // Y-axis label (max value).
  if (opts.yLabel) {
    ctx.fillStyle = "#8b8fa3";
    ctx.font = "10px monospace";
    ctx.textAlign = "right";
    const allData = series.flatMap(s => s.data || []);
    const max = Math.max(...allData, 1);
    ctx.fillText(opts.yFmt ? opts.yFmt(max) : fmtNum(max), W - 4, 12);
  }
}

// ── Polling ─────────────────────────────────────────────────────────────

async function poll() {
  try {
    const [metricsRes, historyRes, actorsRes] = await Promise.all([
      fetch("/api/metrics"),
      fetch("/api/metrics/history"),
      fetch("/api/actors"),
    ]);
    const snap = await metricsRes.json();
    const history = await historyRes.json();
    const actors = await actorsRes.json();

    // Uptime.
    document.getElementById("uptime").textContent =
      `Uptime: ${fmtUptime(snap.timestamp_secs)} · Polling every ${POLL_MS / 1000}s`;

    // Scheduler stats.
    document.getElementById("tasks-spawned").textContent = fmtNum(snap.tasks_spawned);
    document.getElementById("tasks-completed").textContent = fmtNum(snap.tasks_completed);
    document.getElementById("messages-sent").textContent = fmtNum(snap.messages_sent);
    document.getElementById("active-workers").textContent = snap.active_workers;

    // Memory stats.
    document.getElementById("bytes-live").textContent = fmtBytes(snap.bytes_live);
    document.getElementById("peak-bytes").textContent = fmtBytes(snap.peak_bytes_live);
    document.getElementById("alloc-count").textContent = fmtNum(snap.alloc_count);

    // Rates (computed from previous snapshot).
    if (prevSnap && snap.timestamp_secs > prevSnap.timestamp_secs) {
      const dt = snap.timestamp_secs - prevSnap.timestamp_secs;
      const allocRate = (snap.alloc_count - prevSnap.alloc_count) / dt;
      const msgInRate = (snap.messages_received - prevSnap.messages_received) / dt;
      const msgOutRate = (snap.messages_sent - prevSnap.messages_sent) / dt;
      const stealRate = (snap.steals - prevSnap.steals) / dt;

      document.getElementById("alloc-rate").textContent = fmtRate(allocRate);
      document.getElementById("msg-rate-in").textContent = fmtRate(msgInRate);
      document.getElementById("msg-rate-out").textContent = fmtRate(msgOutRate);
      document.getElementById("steals").textContent = fmtNum(snap.steals);
      document.getElementById("steal-rate").textContent = fmtRate(stealRate);
    }
    prevSnap = snap;

    // Charts from history.
    if (history.length > 1) {
      // Scheduler chart: tasks spawned rate.
      const rates = [];
      for (let i = 1; i < history.length; i++) {
        const dt = history[i].t - history[i - 1].t || 1;
        rates.push((history[i].ts - history[i - 1].ts) / dt);
      }
      drawSparkline("chart-scheduler", [
        { data: rates, color: "rgb(108, 140, 255)", fill: true },
        { data: history.map(h => h.aw), color: "rgb(74, 222, 128)" },
      ], { yLabel: true });

      // Memory chart: bytes live.
      drawSparkline("chart-memory", [
        { data: history.map(h => h.bl), color: "rgb(108, 140, 255)", fill: true },
        { data: history.map(h => h.pb), color: "rgb(248, 113, 113)" },
      ], { yLabel: true, yFmt: fmtBytes });

      // Messages chart: send/recv rates.
      const sendRates = [], recvRates = [];
      for (let i = 1; i < history.length; i++) {
        const dt = history[i].t - history[i - 1].t || 1;
        sendRates.push((history[i].ms - history[i - 1].ms) / dt);
        recvRates.push((history[i].mr - history[i - 1].mr) / dt);
      }
      drawSparkline("chart-messages", [
        { data: sendRates, color: "rgb(108, 140, 255)", fill: true },
        { data: recvRates, color: "rgb(74, 222, 128)" },
      ], { yLabel: true });

      // Allocation timeline: alloc rate.
      const allocRates = [];
      for (let i = 1; i < history.length; i++) {
        const dt = history[i].t - history[i - 1].t || 1;
        allocRates.push((history[i].ac - history[i - 1].ac) / dt);
      }
      drawSparkline("chart-alloc-timeline", [
        { data: allocRates, color: "rgb(251, 191, 36)", fill: true },
      ], { yLabel: true });
    }

    // Actors table.
    const tbody = document.getElementById("actors-tbody");
    if (tbody && actors.length > 0) {
      // Sort by messages processed (descending).
      actors.sort((a, b) => b.msgs - a.msgs);
      let html = "";
      for (const a of actors) {
        const timeMs = (a.time_ns / 1_000_000).toFixed(1);
        const avgUs = a.msgs > 0 ? (a.time_ns / a.msgs / 1000).toFixed(1) : "—";
        const stateColor = a.state === "running" ? "var(--green)" :
                           a.state === "crashed" ? "var(--red)" :
                           a.state === "idle" ? "var(--muted)" : "var(--text)";
        html += `<tr style="border-bottom: 1px solid var(--border);">
          <td style="padding:4px 8px;font-variant-numeric:tabular-nums">${a.id}</td>
          <td style="padding:4px 8px;font-variant-numeric:tabular-nums">${a.pid}</td>
          <td style="padding:4px 8px;color:${stateColor}">${a.state}</td>
          <td style="padding:4px 8px;font-variant-numeric:tabular-nums">${fmtNum(a.msgs)}</td>
          <td style="padding:4px 8px;font-variant-numeric:tabular-nums">${timeMs}</td>
          <td style="padding:4px 8px;font-variant-numeric:tabular-nums">${avgUs}</td>
          <td style="padding:4px 8px;font-variant-numeric:tabular-nums">${a.mbox_depth}</td>
          <td style="padding:4px 8px;font-variant-numeric:tabular-nums">${a.mbox_hwm}</td>
        </tr>`;
      }
      tbody.innerHTML = html;
    } else if (tbody) {
      tbody.innerHTML = '<tr><td colspan="8" style="padding:12px;color:var(--muted)">No actors</td></tr>';
    }
  } catch (e) {
    document.getElementById("uptime").textContent = "Connection lost — retrying...";
  }
}

// Start polling.
poll();
setInterval(poll, POLL_MS);
