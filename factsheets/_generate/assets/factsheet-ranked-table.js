// ── ADA-PARC shared ranked-table renderer ─────────────────────────────────────
// Client-side search / sort / top5-bottom5 for the simplified fact sheet
// template's "States, Ranked" section. One call per table; a sheet with several
// indicators (e.g. Poverty) calls it once per indicator.
//
// The tier shown in the Tier column is COMPUTED SERVER-SIDE by
// fs_assign_tiers() and passed in on each row. This script never derives a tier
// from a breakpoint; that logic lives in factsheet-functions.R so the table,
// the map and the legend can never disagree.
//
// opts = {
//   tableId, bodyId, searchId, toggleId, noResultsId,
//   cols:   [{ label, cls }]              // numeric columns, in display order
//   rows:   [{ name, abbr, rank, tier, tierRank, sort:[Number,...], disp:[String,...] }],
//   tierLabels: { excellent, above, below, poor },
//   toggleLabel: "Show the 5 lowest and 5 highest",
//   restoreLabel: "Show all 51"
// }
function adaparcRankedTable(opts) {
  var table   = document.getElementById(opts.tableId);
  var tbody   = document.getElementById(opts.bodyId);
  var search  = document.getElementById(opts.searchId);
  var toggle  = document.getElementById(opts.toggleId);
  var noRes   = document.getElementById(opts.noResultsId);
  if (!table || !tbody) return;

  var LABELS = opts.tierLabels || {
    excellent: "Excellent", above: "Good", below: "Subpar", poor: "Poor"
  };
  var rows = opts.rows.slice();
  var restricted = false;
  var sortKey = "rank";
  var sortDir = 1;

  function tierChip(r) {
    return '<span class="tier-chip ' + r.tier + '">' + LABELS[r.tier] + '</span>';
  }

  function rowHtml(r) {
    var cells = "";
    for (var i = 0; i < r.disp.length; i++) {
      var cls = (opts.cols[i] && opts.cols[i].cls) || "num";
      cells += '<td class="' + cls + '">' + r.disp[i] + '</td>';
    }
    return '<tr>' +
      '<td class="num">' + r.rank + '</td>' +
      '<td>' + tierChip(r) + '</td>' +
      '<th scope="row">' + r.name +
        (r.abbr ? ' <span class="state-abbr">(' + r.abbr + ')</span>' : '') + '</th>' +
      cells +
      '</tr>';
  }

  function valueFor(r, key) {
    if (key === "rank") return r.rank;
    if (key === "tier") return r.tierRank;
    if (key === "name") return r.name.toLowerCase();
    if (key.charAt(0) === "c") return r.sort[parseInt(key.slice(1), 10)];
    return r.rank;
  }

  function sorted() {
    var out = rows.slice();
    out.sort(function (a, b) {
      var av = valueFor(a, sortKey), bv = valueFor(b, sortKey);
      if (av < bv) return -1 * sortDir;
      if (av > bv) return 1 * sortDir;
      return a.rank - b.rank;
    });
    return out;
  }

  function render() {
    var q = search ? search.value.trim().toLowerCase() : "";
    var view = sorted();
    if (q) {
      view = view.filter(function (r) {
        return r.name.toLowerCase().indexOf(q) !== -1 ||
               (r.abbr && r.abbr.toLowerCase().indexOf(q) !== -1);
      });
    } else if (restricted) {
      view = view.slice(0, 5).concat(view.slice(-5));
    }
    if (view.length === 0) {
      tbody.innerHTML = "";
      if (noRes) noRes.hidden = false;
    } else {
      if (noRes) noRes.hidden = true;
      tbody.innerHTML = view.map(rowHtml).join("");
    }
  }

  if (search) search.addEventListener("input", render);

  if (toggle) {
    toggle.addEventListener("click", function () {
      restricted = !restricted;
      toggle.textContent = restricted
        ? (opts.restoreLabel || "Show all states")
        : (opts.toggleLabel || "Show the 5 lowest and 5 highest");
      toggle.setAttribute("aria-expanded", restricted ? "false" : "true");
      if (search) search.value = "";
      render();
    });
  }

  table.querySelectorAll("th.sortable").forEach(function (th) {
    th.addEventListener("click", function () {
      var key = th.getAttribute("data-sort");
      if (sortKey === key) {
        sortDir = -sortDir;
      } else {
        sortKey = key;
        sortDir = 1;
      }
      table.querySelectorAll("th.sortable .sort-arrow").forEach(function (a) {
        a.textContent = "";
      });
      var arrow = th.querySelector(".sort-arrow");
      if (arrow) arrow.textContent = sortDir === 1 ? "▲" : "▼";
      th.setAttribute("aria-sort", sortDir === 1 ? "ascending" : "descending");
      render();
    });
  });

  render();
}
window.adaparcRankedTable = adaparcRankedTable;
