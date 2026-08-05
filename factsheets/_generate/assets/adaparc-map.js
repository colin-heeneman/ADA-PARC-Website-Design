// ── ADA-PARC shared map palette & renderer ─────────────────────
// ADA-PARC HERITAGE tier fills, adopted 2026-07-28.
//
// This file runs in the browser and cannot call R, so these values MIRROR
// ADA_PARC_PALETTE$fill in scripts/palettes.R, which is the single source of
// truth shared with the national map, the city map and both scorecards.
//
// Do not edit these by hand. Edit scripts/palettes.R, then run
//   python3 scripts/check_palette.py
// which fails if this file has drifted, then regenerate the fact sheets.
const ADAPARC_TIER_COLOR = {
  excellent: "#560000",  // deep maroon  (Excellent, best performance)
  above:     "#a52f1a",  // dark red     (Above Average)
  below:     "#d88036",  // burnt orange (Below Average)
  poor:      "#fdd182",  // warm gold    (Poor, worst performance)
  na:        "#e0e0e0"
};

// Per-state border ink, adopted 2026-08-04. Mirrors ADA_PARC_PALETTE$on_fill
// (and $na_text for the no-data fill) in scripts/palettes.R, and is checked by
// scripts/check_palette.py alongside the fills above.
//
// The border used to be a fixed white on every state. That is invisible where
// it meets either of the two light tiers, so neighbouring gold or orange states
// merged into one undifferentiated patch. Each state now takes the ink that its
// OWN fill calls for: near-black on the two light tiers, white on the two dark
// ones. Same rule as border_ink_for_fill() on the national map.
//
// on_fill is held to the 4.5:1 AA text floor, which is stricter than the 3:1
// WCAG 2.2 SC 1.4.11 floor a border has to clear, so reading it here is safe.
const ADAPARC_TIER_BORDER = {
  excellent: "#ffffff",
  above:     "#ffffff",
  below:     "#111111",
  poor:      "#111111",
  na:        "#444444"
};

// National perimeter. Drawn UNDER the states, wider than the per-state stroke,
// so each state's fill paints over its inner half and only the outside flank
// survives. Without it, a dark-tier coastal state's white border vanishes
// against the white page and the national silhouette breaks up.
const ADAPARC_MAP_PERIMETER = "#1c2b3a";  // = ADA_PARC_PALETTE$stroke

const ADAPARC_TIER_LABEL = {
  excellent: "Excellent",
  above:     "Above Average",
  below:     "Below Average",
  poor:      "Poor",
  na:        "No data available"
};

// Neutral alternative for indicators where a high or low value is not a better
// or worse outcome, only a larger or smaller one. Non-institutional group
// quarters is the motivating case: the category mixes group homes, shelters and
// college dormitories, so calling any quartile "Excellent" would assert
// something the data does not support. The color ramp still encodes magnitude
// (darker = higher), but no label carries a value judgment.
const ADAPARC_TIER_LABEL_MAGNITUDE = {
  excellent: "Highest quarter",
  above:     "Second highest quarter",
  below:     "Second lowest quarter",
  poor:      "Lowest quarter",
  na:        "No data available"
};

// FIPS -> full state name
const NAMES = {
  "01":"Alabama","02":"Alaska","04":"Arizona","05":"Arkansas","06":"California",
  "08":"Colorado","09":"Connecticut","10":"Delaware","11":"District of Columbia",
  "12":"Florida","13":"Georgia","15":"Hawaii","16":"Idaho","17":"Illinois",
  "18":"Indiana","19":"Iowa","20":"Kansas","21":"Kentucky","22":"Louisiana",
  "23":"Maine","24":"Maryland","25":"Massachusetts","26":"Michigan","27":"Minnesota",
  "28":"Mississippi","29":"Missouri","30":"Montana","31":"Nebraska","32":"Nevada",
  "33":"New Hampshire","34":"New Jersey","35":"New Mexico","36":"New York",
  "37":"North Carolina","38":"North Dakota","39":"Ohio","40":"Oklahoma",
  "41":"Oregon","42":"Pennsylvania","44":"Rhode Island","45":"South Carolina",
  "46":"South Dakota","47":"Tennessee","48":"Texas","49":"Utah","50":"Vermont",
  "51":"Virginia","53":"Washington","54":"West Virginia","55":"Wisconsin","56":"Wyoming"
};

let __topoCache = null;
async function __getTopoJSON() {
  if (__topoCache) return __topoCache;
  const r = await fetch("https://cdn.jsdelivr.net/npm/us-atlas@3/states-10m.json");
  if (!r.ok) throw new Error("Failed to fetch map data");
  __topoCache = await r.json();
  return __topoCache;
}

// Build or reuse a floating tooltip element attached to the map container.
function __ensureTooltip(container, liveId) {
  let tip = container.querySelector(".map-tooltip");
  if (!tip) {
    tip = document.createElement("div");
    tip.className = "map-tooltip";
    tip.setAttribute("role", "tooltip");
    tip.setAttribute("aria-hidden", "true");
    container.appendChild(tip);
  }
  let live = document.getElementById(liveId);
  if (!live) {
    live = document.createElement("div");
    live.id = liveId;
    live.className = "sr-only";
    live.setAttribute("aria-live", "polite");
    live.setAttribute("aria-atomic", "true");
    container.appendChild(live);
  }
  return { tip, live };
}

/**
 * Render an interactive, hover/keyboard-accessible choropleth.
 * @param {object} opts
 * @param {string} opts.containerId   DOM id of the map container
 * @param {string} opts.descId        DOM id of the 'Loading…' paragraph to remove
 * @param {object} opts.stateData     { FIPS -> [tier, displayValue] }
 * @param {string} opts.svgTitle      accessible <title>
 * @param {string} opts.svgDesc       accessible <desc>
 * @param {string} opts.valueLabel    short label describing the value (e.g. "Poverty rate")
 * @param {object} [opts.tierLabels]  optional tier -> label map. Defaults to the
 *                                    performance labels. Pass
 *                                    ADAPARC_TIER_LABEL_MAGNITUDE for indicators
 *                                    with no better/worse direction.
 */
async function adaparcRenderMap(opts) {
  const { containerId, descId, stateData, svgTitle, svgDesc, valueLabel } = opts;
  const LABELS = opts.tierLabels || ADAPARC_TIER_LABEL;
  const container = document.getElementById(containerId);
  const descEl    = descId ? document.getElementById(descId) : null;
  if (!container) return;

  try {
    const us = await __getTopoJSON();
    const W = 880, H = 540;
    container.innerHTML = "";

    const { tip, live } = __ensureTooltip(container, containerId + "-live");

    const svg = d3.select(container)
      .append("svg")
      .attr("viewBox", `0 0 ${W} ${H}`)
      .attr("width", "100%")
      .attr("role", "img")
      .attr("aria-labelledby", `${containerId}-svgtitle ${containerId}-svgdesc`);

    svg.append("title")
      .attr("id", `${containerId}-svgtitle`)
      .text(svgTitle);
    svg.append("desc")
      .attr("id", `${containerId}-svgdesc`)
      .text(svgDesc);

    const proj    = d3.geoAlbersUsa().scale(1080).translate([W / 2, H / 2]);
    const pathGen = d3.geoPath().projection(proj);
    const states  = topojson.feature(us, us.objects.states);

    function showInfo(fips, el, evt) {
      const name = NAMES[fips] || "Unknown state";
      const row  = stateData[fips];
      const tier = row ? row[0] : "na";
      const val  = row ? row[1] : "N/A";
      const tierLabel = LABELS[tier] || "No data available";
      tip.innerHTML =
          '<span class="mt-name">' + name + '</span>' +
          '<span class="mt-value">' + (valueLabel || "Value") + ': ' + val + '</span>' +
          '<span class="mt-tier ' + tier + '">' + tierLabel + '</span>';
      tip.setAttribute("aria-hidden", "false");
      tip.dataset.visible = "true";

      // Position tooltip
      const rect = container.getBoundingClientRect();
      let x, y;
      if (evt && (evt.clientX !== undefined)) {
        x = evt.clientX - rect.left;
        y = evt.clientY - rect.top;
      } else if (el) {
        const b = el.getBoundingClientRect();
        x = b.left - rect.left + b.width / 2;
        y = b.top  - rect.top;
      } else {
        x = rect.width / 2;
        y = 40;
      }
      // Clamp so tip stays inside the container
      const minX = 60, maxX = rect.width - 60;
      if (x < minX) x = minX;
      if (x > maxX) x = maxX;
      tip.style.left = x + "px";
      tip.style.top  = y + "px";

      live.textContent = name + ', ' + (valueLabel || 'value') + ' ' + val + ', ' + tierLabel + '.';
    }
    function hideInfo() {
      tip.dataset.visible = "false";
      tip.setAttribute("aria-hidden", "true");
    }

    // National perimeter, appended BEFORE the states so it sits underneath.
    // topojson.mesh with (a, b) => a === b returns only the arcs that belong to
    // a single polygon, i.e. the coastline and the international borders.
    svg.append("path")
      .datum(topojson.mesh(us, us.objects.states, (a, b) => a === b))
      .attr("class", "map-perimeter")
      .attr("fill", "none")
      .attr("stroke", ADAPARC_MAP_PERIMETER)
      .attr("stroke-width", 1.4)
      .attr("stroke-linejoin", "round")
      .attr("d", pathGen)
      .attr("aria-hidden", "true")
      .style("pointer-events", "none");

    svg.selectAll("path.state-path")
      .data(states.features)
      .join("path")
      .attr("class", "state-path")
      .attr("d", pathGen)
      .attr("tabindex", 0)
      .attr("fill", d => {
        const fips = String(d.id).padStart(2, "0");
        const row  = stateData[fips];
        return row ? ADAPARC_TIER_COLOR[row[0]] : ADAPARC_TIER_COLOR.na;
      })
      .attr("stroke", d => {
        const fips = String(d.id).padStart(2, "0");
        const row  = stateData[fips];
        return row ? ADAPARC_TIER_BORDER[row[0]] : ADAPARC_TIER_BORDER.na;
      })
      .attr("stroke-width", 0.6)
      .attr("role", "button")
      .attr("aria-label", d => {
        const fips = String(d.id).padStart(2, "0");
        const name = NAMES[fips] || "Unknown state";
        const row  = stateData[fips];
        const val  = row ? row[1] : "N/A";
        const tier = row ? LABELS[row[0]] : LABELS.na;
        return name + ': ' + (valueLabel || 'value') + ' ' + val + ', ' + tier + '.';
      })
      .each(function(d) {
        // Keep a native SVG <title> for baseline browser/AT support
        const fips = String(d.id).padStart(2, "0");
        const name = NAMES[fips] || "Unknown state";
        const row  = stateData[fips];
        const val  = row ? row[1] : "N/A";
        const tier = row ? LABELS[row[0]] : LABELS.na;
        d3.select(this).append("title")
          .text(name + ': ' + val + ' (' + tier + ')');
      })
      .on("mouseenter", function(event, d) {
        const fips = String(d.id).padStart(2, "0");
        d3.select(this).raise().classed("is-active", true);
        showInfo(fips, this, event);
      })
      .on("mousemove", function(event, d) {
        const fips = String(d.id).padStart(2, "0");
        showInfo(fips, this, event);
      })
      .on("mouseleave", function() {
        d3.select(this).classed("is-active", false);
        hideInfo();
      })
      .on("focus", function(event, d) {
        const fips = String(d.id).padStart(2, "0");
        d3.select(this).raise().classed("is-active", true);
        showInfo(fips, this, null);
      })
      .on("blur", function() {
        d3.select(this).classed("is-active", false);
        hideInfo();
      });

    // NOTE: an interior mesh used to be drawn here, a single white path over
    // every shared boundary. It is gone deliberately. A mesh is one path and so
    // can carry only one colour, which is exactly the fixed white the per-state
    // ink above replaces; leaving it in would paint over every border the ink
    // just corrected. Interior boundaries are now held by the state strokes
    // themselves.

    if (descEl) descEl.remove();

  } catch (err) {
    container.innerHTML =
      '<div class="map-error" role="alert">Map could not be loaded (' + err.message +
      '). All data is available in the table below.</div>';
  }
}
// Expose globally for per-factsheet code blocks.
window.adaparcRenderMap = adaparcRenderMap;
window.ADAPARC_TIER_COLOR = ADAPARC_TIER_COLOR;
window.ADAPARC_TIER_LABEL = ADAPARC_TIER_LABEL;
window.ADAPARC_TIER_LABEL_MAGNITUDE = ADAPARC_TIER_LABEL_MAGNITUDE;
