// Kleine Hilfsfunktionen für alle Szenen. Keine Zufallszahlen, keine Uhren.
window.__LIB = (() => {
  const SVG = "http://www.w3.org/2000/svg";
  const el = (tag, attrs = {}, parent = null) => {
    const n = document.createElementNS(SVG, tag);
    for (const [k, v] of Object.entries(attrs)) n.setAttribute(k, v);
    if (parent) parent.appendChild(n);
    return n;
  };
  // Pfad für eine Kurve y(x), x in [0,1], y in [0,1]; Box (x0,y0,w,h), y nach oben
  const curvePath = (ys, x0, y0, w, h, close = false) => {
    const n = ys.length;
    let d = "";
    ys.forEach((y, i) => {
      const x = x0 + (w * i) / (n - 1);
      const yy = y0 + h - h * y;
      d += (i === 0 ? "M" : "L") + x.toFixed(1) + " " + yy.toFixed(1);
    });
    if (close) d += ` L${(x0 + w).toFixed(1)} ${(y0 + h).toFixed(1)} L${x0.toFixed(1)} ${(y0 + h).toFixed(1)} Z`;
    return d;
  };
  const fmtDe = (x, digits = 3) =>
    x.toLocaleString("de-DE", { minimumFractionDigits: digits, maximumFractionDigits: digits });
  const fmtInt = (x) => Math.round(x).toLocaleString("de-DE").replace(/\./g, " ");
  const pct = (p) => Math.round(p * 100) + " %";
  // Höhe der Beta(7,4)-Landschaft an Stelle p, normiert auf 1
  const height = (p) => {
    const c = window.__DATA.curve;
    const i = Math.min(c.length - 1, Math.max(0, p * (c.length - 1)));
    const lo = Math.floor(i), hi = Math.min(c.length - 1, lo + 1);
    return c[lo] + (c[hi] - c[lo]) * (i - lo);
  };
  const dashDraw = (path) => {
    const len = path.getTotalLength ? path.getTotalLength() : 2000;
    path.setAttribute("stroke-dasharray", len);
    path.setAttribute("stroke-dashoffset", len);
    return len;
  };
  return { el, curvePath, fmtDe, fmtInt, pct, height, dashDraw };
})();
