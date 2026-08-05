/* =============================================================================
 * build_factsheet_prose_docx.js
 *
 * ONE-TIME SCAFFOLD. Generates docs/factsheet-prose.docx, the shared editorial
 * document that houses the explanatory prose for all ten ADA-PARC fact sheets.
 *
 * This runs once. After the document is circulated, the humans editing it own
 * it, and re-running this script would destroy their work. It is committed for
 * provenance, so the starting state of the document is traceable, not because
 * it belongs in a render pipeline.
 *
 * The ongoing direction, docx -> factsheet-content.yml, lives in
 * scripts/prose_docx_to_yml.R and IS part of the pipeline.
 *
 * Prose for the three sheets that carry forward (Access to Housing, HCBS and
 * Poverty) is lifted verbatim from factsheets/_generate/content/factsheet-content.yml
 * so nothing client-approved gets retyped. Prose for Living in Community and
 * Living in Institution is new; the retiring Nursing Home sheet's Olmstead
 * framing is carried into the latter. The remaining five sheets are scaffolded
 * with headings only. Employment, Health Insurance Access, Commuting to Work
 * and Educational Attainment were drafted on 30 July 2026. Technology Access
 * remains a scaffold; see docs/community-living-handoff.md for why.
 *
 * Run:  node scripts/build_factsheet_prose_docx.js
 * Deps: docx (npm). Node only because the docx JS library is the tested path;
 *       nothing else in this project depends on Node.
 * ========================================================================== */

const fs = require("fs");
const path = require("path");
const {
  Document, Packer, Paragraph, TextRun, HeadingLevel, AlignmentType,
  BorderStyle, PageOrientation, TableOfContents, PageBreak, ExternalHyperlink
} = require("docx");

const MAROON = "7B1717";
const GOLD   = "A16207";
const INK    = "111111";
const SOFT   = "3D3630";
const FONT   = "EB Garamond";

/* ---------------------------------------------------------------------------
 * Inline markup helper.
 *
 * Editors work in Word with real bold, italic and superscript, not HTML tags.
 * The converter turns that formatting back into <strong>, <em> and <sup> when
 * it writes the YAML. Here we go the other way: the small subset of HTML used
 * in the existing content file becomes real Word runs.
 * ------------------------------------------------------------------------ */
function runs(html, base = {}) {
  const out = [];
  // <a href> becomes a real Word hyperlink so the URL survives the round trip
  // back into the generator's HTML. Anything else is a formatting run.
  const re = /<a\s+href="([^"]+)"[^>]*>([\s\S]*?)<\/a>|<(strong|em|sup)>([\s\S]*?)<\/\3>/g;
  let last = 0, m;
  const push = (text, extra) => {
    if (!text) return;
    out.push(new TextRun({
      text: decode(text), font: FONT, size: 22, color: INK, ...base, ...extra
    }));
  };
  while ((m = re.exec(html)) !== null) {
    push(html.slice(last, m.index));
    if (m[1]) {
      out.push(new ExternalHyperlink({
        link: m[1],
        children: [new TextRun({
          text: decode(m[2]), font: FONT, size: 22, color: "7B1717",
          underline: {}, ...base
        })]
      }));
    } else {
      if (m[3] === "strong") push(m[4], { bold: true });
      if (m[3] === "em")     push(m[4], { italics: true });
      if (m[3] === "sup")    push(m[4], { superScript: true });
    }
    last = re.lastIndex;
  }
  push(html.slice(last));
  return out;
}

function decode(s) {
  return s
    .replace(/<[^>]+>/g, "")
    .replace(/&ndash;/g, "–")
    .replace(/&times;/g, "×")
    .replace(/&lt;/g, "<").replace(/&gt;/g, ">")
    .replace(/&ldquo;/g, "“").replace(/&rdquo;/g, "”")
    .replace(/&rsquo;/g, "’").replace(/&sect;/g, "§")
    .replace(/&amp;/g, "&");
}

/* ---- paragraph builders -------------------------------------------------- */
const body = (html) => new Paragraph({
  children: runs(html), spacing: { after: 160, line: 276 }
});

const note = (text) => new Paragraph({
  children: [new TextRun({ text, font: FONT, size: 20, italics: true, color: SOFT })],
  spacing: { after: 160 }
});

const placeholder = (text) => new Paragraph({
  children: [new TextRun({ text, font: FONT, size: 20, italics: true, color: GOLD })],
  spacing: { after: 160 }
});

const h1 = (title, key) => new Paragraph({
  heading: HeadingLevel.HEADING_1,
  pageBreakBefore: true,
  spacing: { before: 200, after: 80 },
  border: { bottom: { style: BorderStyle.SINGLE, size: 12, color: MAROON, space: 4 } },
  children: [
    new TextRun({ text: title, font: FONT, size: 34, bold: true, color: MAROON }),
    new TextRun({ text: `   [key: ${key}]`, font: FONT, size: 20, color: SOFT })
  ]
});

const h2 = (text) => new Paragraph({
  heading: HeadingLevel.HEADING_2,
  spacing: { before: 260, after: 100 },
  children: [new TextRun({ text, font: FONT, size: 26, bold: true, color: MAROON })]
});

const h3 = (title, key) => new Paragraph({
  heading: HeadingLevel.HEADING_3,
  spacing: { before: 200, after: 80 },
  children: [
    new TextRun({ text: title, font: FONT, size: 23, bold: true, color: INK }),
    new TextRun({ text: `   [key: ${key}]`, font: FONT, size: 18, color: SOFT })
  ]
});

const category = (name) => new Paragraph({
  spacing: { after: 200 },
  children: [new TextRun({
    text: `Category: ${name}`, font: FONT, size: 20, italics: true, color: GOLD
  })]
});

const bullet = (text) => new Paragraph({
  bullet: { level: 0 }, spacing: { after: 90 },
  children: runs(text, { size: 21 })
});

// `instance` restarts the numbering. Without it Word continues one list across
// the whole document, so the tenth sheet's first footnote renders as "28." while
// the body text beside it cites <sup>1</sup>. One instance per sheet keeps the
// visible number and the marker in agreement.
const numbered = (text, instance) => new Paragraph({
  numbering: { reference: "footnote-numbers", level: 0, instance },
  spacing: { after: 110 },
  children: runs(text, { size: 21 })
});

/* =============================================================================
 * CONTENT
 * ========================================================================== */

const sheets = [
  /* ---------------------------------------------------------------- 1 ----- */
  {
    key: "living_community",
    title: "Living in Community",
    category: "Community Living",
    intro: [
      "The Americans with Disabilities Act (ADA) rests on the principle that people with disabilities have the right to live in the community, with the supports they need, rather than in an institution. The U.S. Supreme Court affirmed that right in the landmark <em>Olmstead</em> decision in 1999, holding that unnecessary institutionalization is a form of discrimination.<sup>1</sup>",
      "This fact sheet shows how many people with disabilities are living in the community in each state. Community living covers two situations. Most people live <strong>at home</strong>, alone or with family. A smaller number live in <strong>non-institutional group quarters</strong>, a Census Bureau category for group settings that are not institutions, such as group homes, shelters and college dormitories.<sup>2</sup>",
      "Nationally, <strong>95.5 percent</strong> of people with disabilities live in the community, compared with <strong>99.5 percent</strong> of people without disabilities. The maps and tables below show how much that figure varies from state to state.",
      "Higher percentages mean more people with disabilities are living in the community. The residence categories on this sheet and on its companion, <em>Living in Institution</em>, are measured against the same population and add to 100 percent. Every percentage point not living in the community is a percentage point living in an institution."
    ],
    sections: [
      { key: "community_combined", name: "Living in the Community (combined)",
        lead: ["The map below combines both community settings, people living at home and people living in non-institutional group quarters, into a single measure. States shaded darker have a larger share of their disabled residents living in the community."] },
      { key: "home", name: "Living at Home",
        lead: ["Living at home means living in a house or apartment, alone, with family or with roommates, rather than in any kind of group setting. This is by far the most common living arrangement for people with disabilities, and it is the measure most directly tied to the independent living the ADA envisions."] },
      { key: "nonintgq", name: "Non-Institutional Group Quarters",
        lead: ["Non-institutional group quarters is a Census Bureau category covering group living arrangements that are not institutions. It includes group homes and supported living for people with disabilities, but also emergency shelters, transitional housing and college dormitories.",
               "Because the category mixes supported community living with housing instability, a higher percentage is not straightforwardly better or worse. This map is therefore shaded by how large the share is rather than by performance, and its groups are labeled by size rather than as Excellent or Poor. Read it alongside the other two measures rather than on its own."] }
    ],
    footnotes: [
      "<em>Olmstead v. L.C.</em>, 527 U.S. 581 (1999). See also Office for Civil Rights. (2018, June 28). <em>Serving people with disabilities in the most integrated setting: Community living and Olmstead.</em> U.S. Department of Health and Human Services.",
      "U.S. Census Bureau. American Community Survey 5-year estimates ({acs_start}&ndash;{acs_end}). Tables ACSST5Y{acs_year}.S2601A and ACSDT5Y{acs_year}.B26108. All percentages on this sheet are calculated against the total population with a disability, including people living in institutions."
    ]
  },

  /* ---------------------------------------------------------------- 2 ----- */
  {
    key: "living_institution",
    title: "Living in Institution",
    category: "Community Living",
    intro: [
      "The Americans with Disabilities Act (ADA) mandates the right to live in the community with supports instead of living in a nursing home or other institution. The U.S. Supreme Court upheld this right in the landmark <em>Olmstead</em> decision in 1999.<sup>1</sup>",
      "Despite that right, people with disabilities remain far more likely than other people to live in an institution. Nationally, <strong>4.6 percent</strong> of people with disabilities live in an institutional setting, compared with <strong>0.5 percent</strong> of people without disabilities. People with disabilities are <strong>more than eight times</strong> as likely to be institutionalized.",
      "The Census Bureau counts three kinds of institutional settings in these data. <strong>Nursing homes and other medical facilities</strong> account for the largest share, followed by <strong>adult correctional facilities</strong>. A small remainder covers settings such as psychiatric hospitals and juvenile facilities.<sup>2</sup>",
      "Lower percentages mean fewer people with disabilities are institutionalized and more are living in the community. Some states do considerably better than others. Where a person lives continues to shape whether they are supported at home or placed in an institution."
    ],
    sections: [
      { key: "inst_combined", name: "Living in an Institution (combined)",
        lead: ["This map combines every institutional setting into one measure, the share of people with disabilities living in an institution rather than in the community. It is the exact complement of the combined measure on the companion sheet, <em>Living in Community</em>."] },
      { key: "nursing", name: "Nursing Homes and Medical Facilities",
        lead: ["Nursing homes and other long-term medical facilities are the most common institutional setting for people with disabilities. This measure covers residents of all ages. Rates broken out by age group appear in the supplementary table further down."] },
      { key: "corrections", name: "Correctional Facilities",
        lead: ["Adult correctional facilities include prisons and prison farms, reception and diagnostic centers, and facilities for parole violators. People with disabilities are over-represented in these settings relative to their share of the general population, a pattern that reflects gaps in community mental health services, housing and support as much as anything about the criminal legal system itself.<sup>3</sup>"] },
      { key: "nursing_by_age", name: "Nursing Home Rates by Age (supplementary table)",
        lead: ["The table below breaks nursing home residence into two age groups. <strong>These figures use different denominators from the maps above.</strong> The working-age rate is calculated against the population with a disability aged 18 to 64, and the rate for older adults against the population with a disability aged 65 and over. They describe the risk within each age group. They cannot be added together, and they cannot be compared against the all-ages figures on the maps."] }
    ],
    footnotes: [
      "Office for Civil Rights. (2018, June 28). <em>Serving people with disabilities in the most integrated setting: Community living and Olmstead.</em> U.S. Department of Health and Human Services.",
      "U.S. Census Bureau. American Community Survey 5-year estimates ({acs_start}&ndash;{acs_end}). Tables ACSST5Y{acs_year}.S2601A, ACSST5Y{acs_year}.S2602 and ACSDT5Y{acs_year}.B26108. The institutional total is derived as the total population with a disability less those living at home and those living in non-institutional group quarters.",
      "Bureau of Justice Statistics. Adult correctional facilities include prisons and prison farms; reception, diagnostic and classification centers; facilities for parole violators; and road camps."
    ]
  },

  /* ---------------------------------------------------------------- 3 ----- */
  {
    key: "housing",
    title: "Access to Housing",
    category: "Community Living",
    carried: true,
    intro: [
      "The Americans with Disabilities Act (ADA) mandates the right to live in the community with supports for people with disabilities instead of living in a nursing home. Similarly, the Fair Housing Act (FHA) expands on this right by prohibiting discrimination against people with disabilities by landlords and other housing providers. Despite this right to live in the community, people with disabilities face many difficulties in finding affordable and accessible housing.",
      "Finding and securing housing is a challenge for people with disabilities, as they are far more likely to live in poverty than their non-disabled peers. Additionally, despite protections from the ADA and FHA, people with disabilities still face a great amount of discrimination from landlords. Supports such as the Section 8 Housing Choice Voucher program (HCV) and public housing are meant to alleviate these difficulties.<sup>1</sup>",
      "Some states are doing better than others to ensure access to HCVs and public housing for people with disabilities. To honor the ADA and its mandates of inclusion, states must work to increase the representation of people with disabilities in their housing support, especially the Housing Choice Voucher program, which provides the greatest access to integrated community living."
    ],
    sections: [
      { key: "public_housing", name: "Public Housing", lead: [] },
      { key: "hcv", name: "Housing Choice Vouchers", lead: [] }
    ],
    footnotes: [
      "U.S. Department of Housing and Urban Development (HUD), PD&amp;R, <em>A Picture of Subsidized Households (PSH)</em>, state totals (Summary Level 3), Public Housing (program=2) and Housing Choice Vouchers (program=3)."
    ]
  },

  /* ---------------------------------------------------------------- 4 ----- */
  {
    key: "hcbs",
    title: "Spending on Home & Community Based Services",
    category: "Community Living",
    carried: true,
    intro: [
      "The Americans with Disabilities Act (ADA) mandates the right to live in the community with supports instead of living in a nursing home or long-term care institution. This right was upheld by the U.S. Supreme Court in the landmark <em>Olmstead</em> decision in 1999.<sup>1</sup>",
      "Despite this right to live in the community, some states invest more than others in each person they serve. The map and tables below display <strong>state Medicaid spending per recipient on home- and community-based services (HCBS)</strong>, inclusive of all waiver types (Community First Choice, Section 1915(i), Section 1915(c), Section 1515), for people with disabilities (all ages) living in the community. This is the total HCBS expenditure divided by the number of people receiving HCBS.<sup>2</sup>",
      "Lower spending per recipient means fewer financial resources reaching each person with a disability who relies on community services. Higher spending per recipient means stronger financial support for each person's community living. This measure reflects how generously a state supports each recipient, not how many people it reaches. Rebalancing money toward community living remains an ADA-related disparity issue for many people with disabilities, putting them at higher risk for institutionalization. Currently, this community living support is highly dependent on the zip code and state in which you live."
    ],
    sections: [
      { key: "per_recipient", name: "Spending per Recipient", lead: [] },
      { key: "ratio", name: "Community versus Institutional Spending",
        lead: ["Spending per recipient shows how much reaches each person, but not how a state divides its long-term care budget between community and institutional settings. This second measure shows <strong>the share of each state&rsquo;s Medicaid long-term services and supports (LTSS) spending that goes to home- and community-based services (HCBS) rather than to institutions</strong> such as nursing facilities and intermediate care facilities. A higher share means a state directs more of its long-term care dollars toward community living. A lower share means more of its dollars go to institutional care.<sup>3</sup>"] }
    ],
    about: {
      title: "About These Data",
      body: "The information shown on this website comes from research conducted by <strong>Mathematica</strong>. We did not collect or create the original data. We organized and presented the information to make it easier for community members, including people with disabilities, to understand and compare. All credit for the original research, data collection, and analysis belongs to Mathematica and the report&rsquo;s authors. Any changes we made were limited to how the information is displayed and did not alter the original data."
    },
    footnotes: [
      "Office for Civil Rights. (2018, June 28). <em>Serving people with disabilities in the most integrated setting: Community living and Olmstead.</em> U.S. Department of Health and Human Services.",
      "Mathematica, prepared for the Centers for Medicare &amp; Medicaid Services (CMS). <em>Medicaid LTSS Users &amp; Expenditures, {source_year} Data Tables</em> (T-MSIS Analytic Files). Spending per recipient = total HCBS expenditure (A2_LTSSExpDlvrySystm_{source_year}.xlsx, tab &ldquo;A.2.3 All-HCBS&rdquo;, column &ldquo;HCBS (total)&rdquo;) divided by HCBS recipients (A1_LTSSUsrDlvrySystm_{source_year}.xlsx, tab &ldquo;A.1.3 All-HCBS&rdquo;, column &ldquo;HCBS (total)&rdquo;); both inclusive of fee-for-service and managed care. Subpopulation context from C1_LTSSUsrChar_{source_year}.xlsx, tab &ldquo;C.1.21 HCBSBySubpop.&rdquo; See CMS LTSS Reports &amp; Evaluations.",
      "Carpenter, A., Stepanczuk, C., Murray, C., &amp; Wysocki, A. (2025, October 17). <em>Trends in users and expenditures for home and community-based services as a share of total Medicaid long-term services and supports users and expenditures, 2023.</em> Mathematica. <a href=\"https://www.medicaid.gov/medicaid/long-term-services-supports/downloads/ltss-rebalancing-brief-2023.pdf\" target=\"_blank\" rel=\"noopener noreferrer\">Report PDF</a>. Community share of LTSS spending = HCBS expenditure (tab &ldquo;A.2.3 All-HCBS,&rdquo; column &ldquo;HCBS (total)&rdquo;) divided by the sum of HCBS and institutional expenditure (tab &ldquo;A.2.2 All-Inst,&rdquo; column &ldquo;Institutional (total)&rdquo;) from A2_LTSSExpDlvrySystm_{source_year}.xlsx. Because LTSS equals HCBS plus institutional spending, this equals HCBS as a share of total Medicaid LTSS expenditure."
    ]
  },

  /* ---------------------------------------------------------------- 5 ----- */
  {
    key: "poverty",
    title: "Poverty",
    category: "Work & Economic",
    carried: true,
    intro: [
      "The Americans with Disabilities Act (ADA) rests on the idea that people with disabilities should be assured equal opportunity and economic self-sufficiency.<sup>1</sup> Yet, people with disabilities remain over twice as likely to live in poverty.<sup>2</sup>",
      "Despite the ADA's call for economic freedom and equal opportunity, some states are doing better than others to ensure that people with disabilities are not living in poverty. The maps and tables below display <strong>state percentages for working-aged people with disabilities (ages 18&ndash;64) who are below the poverty line</strong>. Higher percentages mean more disabled people are living below the poverty line.",
      "The strain of economic hardship highlights the ways in which disability and poverty remain intertwined. People with disabilities make up less than 13% of the United States population, but over 50% of its people live in long-term poverty.<sup>3</sup> Every state has the responsibility to ensure people with disabilities can live free from poverty."
    ],
    sections: [{ key: "poverty_rate", name: "Poverty Rate, Ages 18 to 64", lead: [] }],
    summary_stats: [
      "2&times; | People with disabilities are more than twice as likely to live in poverty",
      "&lt;13% | of the U.S. population has a disability",
      "&gt;50% | of people with disabilities live in long-term poverty"
    ],
    footnotes: [
      "Americans With Disabilities Act of 1990. Pub. L. 101-336. &sect; 1. 26 July 1990. 104 Stat. 328.",
      "U.S. Census Bureau. American Community Survey 5-year estimates ({acs_start}&ndash;{acs_end}). Table ACSDT5Y{acs_year}.C18130.",
      "National Council on Disability (NCD), 2017 Progress Report. National Council on Disability."
    ]
  },

  /* ------------------------------------------------------- 6 to 10 ------- */
  {
    key: "employment",
    title: "Employment",
    category: "Work & Economic",
    intro: [
      "The Americans with Disabilities Act (ADA) was written to secure equal opportunity and economic self-sufficiency for people with disabilities, and Title I of the Act specifically prohibits discrimination in employment.<sup>1</sup> More than three decades on, the gap in who holds a job remains one of the widest disparities in this data.",
      "Nationally, <strong>43 percent</strong> of working-age people with disabilities are employed, compared with <strong>78 percent</strong> of working-age people without disabilities. Put another way, a person without a disability is close to <strong>twice as likely</strong> to be working.",
      "This fact sheet shows the three employment situations the Census Bureau distinguishes for people aged 18 to 64. A person is <strong>employed</strong>, <strong>unemployed</strong> (not working but actively looking for work), or <strong>not in the labor force</strong> (neither working nor looking). Everyone is counted in exactly one of the three, so for each group the shares add to 100 percent.<sup>2</sup> These figures were checked against the source Census table directly, and reconcile exactly.<sup>3</sup>",
      "The largest difference is not in unemployment but in labor force participation. Just under half of working-age people with disabilities are in the labor force at all, against more than four in five people without disabilities. Barriers to getting and keeping a job, including inaccessible workplaces, transportation, benefit rules that penalize earnings, and outright discrimination, show up here rather than in the unemployment figure."
    ],
    sections: [
      { key: "employed", name: "Employed",
        lead: ["The share of working-age people with disabilities who are employed. This is the headline measure of whether a state's economy is open to disabled workers, and it varies widely, from roughly a third to well over half depending on where a person lives."] },
      { key: "unemployed", name: "Unemployed",
        lead: ["The share of <em>all</em> working-age people with disabilities who are unemployed, meaning not working but actively looking for work. Because the denominator is everyone in the age group rather than only those in the labor force, this figure is not the unemployment rate as it is usually reported. Both measures appear in the table further down, and they tell different stories."] },
      { key: "notlabor", name: "Not in the Labor Force",
        lead: ["The share of working-age people with disabilities who are neither working nor looking for work. This is the largest of the three categories for disabled people and the smallest for everyone else, which is the central finding of this sheet.",
               "The category covers a range of situations, including study, caregiving, and retirement before 65, alongside people who have stopped looking after sustained difficulty finding work. It is shaded so that lower shares read as the more favorable outcome, on the reasoning that a person who wants to work should be able to. That framing will not fit every individual it counts."] },
      { key: "rates_table", name: "Labor Force Participation and Unemployment Rate (supplementary table)",
        lead: ["The maps above measure each category against everyone aged 18 to 64. The table below reports the two figures that are more commonly quoted, and <strong>they use different denominators</strong>. The <strong>labor force participation rate</strong> is the share of the age group either working or looking. The <strong>unemployment rate</strong> is the share <em>of those in the labor force</em> who are not working, which is the definition the Bureau of Labor Statistics uses.",
               "The distinction matters. Nationally the unemployment rate for people with disabilities is <strong>10.9 percent</strong> against <strong>4.8 percent</strong> for people without, a gap of roughly two to one. The share of all disabled working-age people who are unemployed is only 5.3 percent, because so many are not in the labor force to begin with. Neither figure is wrong; they answer different questions, and quoting one while meaning the other is the most common way this data gets misread."] }
    ],
    footnotes: [
      "Americans With Disabilities Act of 1990. Pub. L. 101-336. &sect; 1 and Title I. 26 July 1990. 104 Stat. 328.",
      "U.S. Census Bureau. American Community Survey 5-year estimates ({acs_start}&ndash;{acs_end}). Table ACSDT5Y{acs_year}.C18120, Employment Status by Disability Status. Universe: civilian noninstitutionalized population aged 18 to 64. Each share is calculated over the sum of the three C18120 categories for the same disability group, so the categories are mutually exclusive and sum to 100 percent.",
      "These values were independently verified in July 2026 against a fresh pull of Table C18120 from the Census Bureau API, reconciling exactly across all national, state and place records. See <em>ADA-PARC Employment Indicator Audit</em> and <code>scripts/audit_employment_acs.R</code>."
    ]
  },
  {
    key: "education",
    title: "Educational Attainment",
    category: "Community Participation",
    intro: [
      "Education shapes almost everything that follows it: the work available to a person, what it pays, and how much choice they have about where and how they live. The gap in educational attainment between disabled and non-disabled adults is one of the widest in this data, and it opens early.",
      "Among adults aged 25 and over, <strong>17.0 percent</strong> of people with disabilities have not completed high school, against <strong>8.9 percent</strong> of people without. At the other end, <strong>20.9 percent</strong> hold a bachelor's degree or higher, against <strong>39.1 percent</strong>. On both measures the disparity is close to two to one, in opposite directions.<sup>1</sup>",
      "A note on what these three measures do and do not cover. They are not a complete account of educational attainment. People whose highest qualification is some college without a degree, or an associate's degree, are in none of the three, and they are a large group: <strong>29 percent</strong> of adults with disabilities and 28 percent of adults without. The three shares therefore add to between 63 and 79 percent depending on the state, not to 100.<sup>2</sup>",
      "The final table on this sheet reports that remainder, so the full picture is visible rather than implied."
    ],
    sections: [
      { key: "no_hs", name: "Less than High School",
        lead: ["The share of adults aged 25 and over with a disability whose formal education ended before a high school diploma or its equivalent. This is the clearest of the three measures, and the one most closely tied to later poverty and unemployment. It ranges from about 9 percent to 23 percent depending on the state."] },
      { key: "hs", name: "High School Diploma or Equivalent",
        lead: ["The share whose highest qualification is a high school diploma or GED. This map is shaded by <strong>how large the share is</strong> rather than by performance, because the measure reads two ways at once. A high figure can mean a state gets more of its disabled residents through high school, and it can mean more of them stop there.",
               "Read it against the two maps either side, which do carry a direction. A state with a large share here and a small share below high school is doing something different from a state with a large share here and a small share holding degrees."] },
      { key: "bachelors", name: "Bachelor's Degree or Higher",
        lead: ["The share holding a bachelor's degree or a higher qualification. Nationally this is 20.9 percent of adults with disabilities against 39.1 percent of adults without, the single largest gap on the sheet. Higher education remains substantially harder to reach and to complete for disabled students, and the state variation reflects both school systems and the accessibility of public universities."] },
      { key: "remainder_table", name: "The Uncounted Middle (table)",
        lead: ["Some college without a degree, and associate's degrees, fall outside all three measures above. This table reports that group directly, calculated as everything the three categories leave out.",
               "It is a substantial share, <strong>29 percent</strong> of adults with disabilities nationally, and it matters for interpretation. A state with a low share holding bachelor's degrees is not necessarily a state where disabled adults stop at high school; many may hold an associate's degree or have attended college without finishing. This table makes that visible."] }
    ],
    footnotes: [
      "U.S. Census Bureau. American Community Survey 5-year estimates ({acs_start}&ndash;{acs_end}). Table ACSST5Y{acs_year}.S1811. Universe: the civilian noninstitutionalized population aged 25 and over. Shares are calculated against that population within each disability group.",
      "The three published categories are not exhaustive. Adults whose highest attainment is some college without a degree, or an associate's degree, are counted in none of them. The share in the final table is derived as 100 percent less the three published shares, so it is exact rather than estimated."
    ]
  },
  {
    key: "health_insurance",
    title: "Health Insurance Access",
    category: "Community Participation",
    intro: [
      "Health insurance is what makes health care reachable, and for people with disabilities it also pays for much of what makes community living possible, including personal care, equipment and long-term services and supports.",
      "This fact sheet begins with a result that runs against expectation. Among working-age adults, people with disabilities are <strong>less</strong> likely to be uninsured than people without disabilities, <strong>9.9 percent</strong> against <strong>12.0 percent</strong>.<sup>1</sup>",
      "The reason is visible in the rest of the data. Public insurance, chiefly Medicaid and Medicare, covers <strong>51.8 percent</strong> of working-age people with disabilities, against <strong>15.0 percent</strong> of people without. Private coverage runs the other way, <strong>49.1 percent</strong> against <strong>76.1 percent</strong>. Public programs are doing the work that employer-sponsored insurance does for everyone else.",
      "That is worth stating plainly, because it is a policy achievement and also a dependency. Coverage that rests on public programs is coverage that moves when eligibility rules, waiver capacity or state budgets move. The state variation on the maps below is largely variation in how far each state has extended those programs.",
      "One note on reading the figures. The four measures are not four slices of a whole. Public and private coverage <strong>overlap</strong>, because a person can hold both, so they add to more than the covered total.<sup>2</sup>"
    ],
    sections: [
      { key: "none_19to64", name: "No Health Insurance, Ages 19 to 64",
        lead: ["The share of working-age people with disabilities who have no health insurance of any kind. This is the clearest performance measure on the sheet, and the one where state policy choices show up most directly. It ranges from about 3 percent to nearly 20 percent depending on the state."] },
      { key: "public_19to64", name: "Public Coverage, Ages 19 to 64",
        lead: ["The share with Medicaid, Medicare or another public plan. This map is shaded by <strong>how large the share is</strong> rather than by performance, because a high figure carries two readings at once. It can mean a state has extended public coverage widely, and it can mean disabled residents are shut out of employer-sponsored insurance and have nowhere else to go. Read it alongside the uninsured map rather than on its own."] },
      { key: "private_19to64", name: "Private Coverage, Ages 19 to 64",
        lead: ["The share with private insurance, most often through an employer. This map is also shaded by size rather than performance, for the mirror-image reason: a high figure may reflect good access to jobs that carry benefits, and a low figure may reflect public programs successfully filling the gap. Note that people can hold private and public coverage at the same time, so this measure and the one above are not mutually exclusive."] },
      { key: "covered_table", name: "Insured and Uninsured, Both Age Groups (table)",
        lead: ["Coverage is the exact complement of no coverage, so it is reported here rather than mapped a second time. The table gives both age groups side by side."] },
      { key: "age65_table", name: "Health Insurance at Age 65 and Over (table)",
        lead: ["Nearly everyone aged 65 and over is insured, because Medicare is close to universal at that age. Coverage runs from <strong>98.5 to 99.9 percent</strong> across the states for people with disabilities, and the uninsured share from 0.1 to 1.5 percent.",
               "That range is too narrow for state ranking to carry meaning, so these measures are shown as a table rather than as maps. Sorting states on a spread of about one percentage point would manufacture a difference that is not there. The figures that do vary at this age are the coverage types, where private insurance still reaches roughly half of older disabled adults as a supplement to Medicare."] }
    ],
    footnotes: [
      "U.S. Census Bureau. American Community Survey 5-year estimates ({acs_start}&ndash;{acs_end}). Table ACSDT5Y{acs_year}.B18135, Health Insurance Coverage Status by Disability Status. Shares for ages 19 to 64 are calculated against the civilian noninstitutionalized population with a disability in that age range; shares for age 65 and over against the corresponding population aged 65 and over.",
      "Public and private coverage are not mutually exclusive in Table B18135. A person with both Medicare and a private supplemental plan is counted in each. For ages 19 to 64 the two shares exceed the insured total by 7 to 14 percentage points, and for age 65 and over by 40 to 62 points, which is why the categories must not be read as parts of a whole."
    ]
  },
  {
    key: "commuting",
    title: "Commuting to Work",
    category: "Community Participation",
    intro: [
      "Getting to work is a precondition for working. Transportation is one of the barriers people with disabilities name most often, and the Americans with Disabilities Act devotes a full title to it, requiring public transit systems to be accessible.<sup>1</sup>",
      "This fact sheet has an unusual shape, and it is worth understanding before reading the maps. The Census question about commuting is asked only of <strong>people who work</strong>. Everything on the maps below therefore describes disabled workers, not disabled people generally.<sup>2</sup>",
      "That distinction carries the sheet's most striking number. Only <strong>23 percent</strong> of people with disabilities are commuting workers, against <strong>51 percent</strong> of people without disabilities. Before any question about how people travel to work, there is a much larger question about who gets to work at all, and that is the subject of the <em>Employment</em> fact sheet.",
      "Among those who do commute, the differences are modest. <strong>3.8 percent</strong> of disabled workers travel by public transit against 3.2 percent of other workers, and <strong>65.4 percent</strong> drive alone against 69.0 percent. The state-to-state variation on the maps is driven far more by whether a place has a transit system than by disability.",
      "The two modes shown here do not add up to everything. Carpooling, walking, cycling, taxis and working from home make up the remainder, and together they account for between a fifth and a half of commutes depending on the state."
    ],
    sections: [
      { key: "transit", name: "Commute by Public Transit",
        lead: ["The share of workers with disabilities who travel to work by public transit. This map is shaded by <strong>how large the share is</strong> rather than by performance, because a high figure has two readings. It can mean a state has accessible transit that people can rely on, and it can mean workers have no alternative because they cannot drive or cannot afford a car.",
               "Density explains most of what the map shows. The District of Columbia and New York are far above every other state; most states sit below 3 percent because they have little public transit for anyone to use, disabled or not."] },
      { key: "drivealone", name: "Commute by Driving Alone",
        lead: ["The share of workers with disabilities who drive to work alone. This map is also shaded by size rather than performance, for the mirror-image reason. Driving alone can reflect independence and a vehicle adapted to a person's needs, and it can reflect the absence of any usable transit.",
               "Workers with disabilities drive alone slightly less often than other workers, 65.4 percent against 69.0 percent nationally, a smaller gap than most people expect."] },
      { key: "workers_table", name: "Who Is Counted as a Commuter (table)",
        lead: ["The table below shows the figure that the maps cannot: what share of each population is in the commuting universe at all. This is the denominator behind every percentage on this sheet.",
               "Nationally it is <strong>23 percent</strong> of people with disabilities against <strong>51 percent</strong> of people without. A state with a low share is not necessarily doing badly on transportation; it may simply have fewer disabled residents in work. Read this table alongside the <em>Employment</em> fact sheet, which measures that directly."] }
    ],
    footnotes: [
      "Americans With Disabilities Act of 1990. Pub. L. 101-336, Title II, Part B and Title III. 26 July 1990. 104 Stat. 328.",
      "U.S. Census Bureau. American Community Survey 5-year estimates ({acs_start}&ndash;{acs_end}). Table ACSST5Y{acs_year}.S1811. Universe: workers aged 16 and over. Mode shares are calculated against the number of workers in each disability group, so they describe how workers travel, not how the whole population travels. The share of each population that is in this worker universe is reported in the final table."
    ]
  },
  {
    key: "technology", title: "Technology Access", category: "Community Participation", scaffold: true,
    sections: [
      { key: "computer",   name: "Computer Access" },
      { key: "internet",   name: "Internet Access" },
      { key: "smartphone", name: "Smartphone Access" }
    ]
  }
];

/* =============================================================================
 * DOCUMENT ASSEMBLY
 * ========================================================================== */

const children = [];

/* ---- cover ---- */
children.push(
  new Paragraph({ spacing: { before: 1400, after: 100 }, children: [
    new TextRun({ text: "ADA-PARC", font: FONT, size: 24, bold: true, color: GOLD,
                  characterSpacing: 60 })] }),
  new Paragraph({ spacing: { after: 120 },
    border: { bottom: { style: BorderStyle.SINGLE, size: 18, color: MAROON, space: 8 } },
    children: [new TextRun({ text: "Fact Sheet Prose", font: FONT, size: 56, bold: true, color: MAROON })] }),
  new Paragraph({ spacing: { after: 400 }, children: [
    new TextRun({ text: "Shared editorial document for the explanatory text on all ten national fact sheets",
                  font: FONT, size: 26, color: SOFT })] }),
  body("This document holds the prose, and only the prose. Every map, table, percentage and ranking on the published fact sheets is calculated from the live Census and Medicaid data by the generator. Nothing you write here changes a number, and nothing a data update changes will overwrite what you write here."),
  body("Three sheets are already published, and their text appears below exactly as it reads today. <strong>Living in Community</strong>, <strong>Living in Institution</strong>, <strong>Employment</strong>, <strong>Health Insurance Access</strong>, <strong>Commuting to Work</strong> and <strong>Educational Attainment</strong> are new and carry draft text for review. Technology Access is scaffolded with headings only; its source data has an unresolved problem, recorded in docs/community-living-handoff.md."),
  new Paragraph({ spacing: { before: 400 }, children: [
    new TextRun({ text: "Prepared 28 July 2026", font: FONT, size: 21, italics: true, color: SOFT })] })
);

/* ---- how to use ---- */
children.push(
  new Paragraph({ pageBreakBefore: true, heading: HeadingLevel.HEADING_1,
    spacing: { after: 120 },
    border: { bottom: { style: BorderStyle.SINGLE, size: 12, color: MAROON, space: 4 } },
    children: [new TextRun({ text: "How to use this document", font: FONT, size: 34, bold: true, color: MAROON })] }),
  body("A script reads this document and converts it into the file the fact sheet generator uses. That means the structure matters as much as the words. A few rules keep the conversion working."),
  h2("Rules"),
  bullet("<strong>Do not rename, reorder or delete headings.</strong> Add and edit the text underneath them freely. If a section is not needed, leave it empty rather than removing its heading."),
  bullet("<strong>Do not touch the bracketed keys</strong> such as [key: living_community]. They are how the script matches your text to the right place on the right sheet."),
  bullet("<strong>Use Word's own bold and italic.</strong> Do not type HTML tags. Bold becomes bold on the fact sheet, italic becomes italic, and superscript becomes a footnote marker."),
  bullet("<strong>Leave the year placeholders alone.</strong> Text such as {acs_start}, {acs_end}, {acs_year}, {source_year} and {pums_year} is filled in automatically with whatever data vintage is loaded when the sheet is built. Typing a year in their place will freeze the citation and it will go stale."),
  bullet("<strong>One idea, one paragraph.</strong> Press Enter for a new paragraph rather than using a line break inside one."),
  bullet("<strong>Footnote markers are superscript numbers</strong> in the body text, and they must match the numbered list under Footnotes on the same sheet."),
  h2("The sections on each sheet"),
  bullet("<strong>Introduction</strong> is required. Two to four paragraphs setting up why the indicator matters and what the reader is about to see."),
  bullet("<strong>Section leads</strong> holds one entry per map on the sheet. Each is a short paragraph introducing that specific map."),
  bullet("<strong>Summary statistics</strong> is optional. One line per statistic, written as the number, then a vertical bar, then the label."),
  bullet("<strong>About These Data</strong> is optional. A single paragraph for data attribution or a methodology note."),
  bullet("<strong>Footnotes</strong> is a numbered list. The order sets the numbering used on the sheet."),
  h2("One thing to check when the data is refreshed"),
  body("Some introductions quote national figures, for example that 95.5 percent of people with disabilities live in the community. Those numbers are typed here, not calculated, so when the fact sheets are rebuilt on a newer Census vintage someone needs to check them against the generator's validation report and update them by hand. They are marked in bold to make them easy to find.")
);

/* ---- sheets ---- */
let sheetIndex = 0;
for (const s of sheets) {
  sheetIndex += 1;
  children.push(h1(s.title, s.key));
  children.push(category(s.category));

  if (s.carried) {
    children.push(note("This sheet is already published. The text below is exactly what appears on it today. Edit freely."));
  }
  if (s.scaffold) {
    children.push(note("This sheet has not been built yet. Headings are in place so the structure is visible; the text will be drafted in a later phase."));
  }

  children.push(h2("Introduction"));
  if (s.intro) s.intro.forEach(p => children.push(body(p)));
  else children.push(placeholder("[To be drafted. Two to four paragraphs.]"));

  children.push(h2("Section leads"));
  for (const sec of s.sections) {
    children.push(h3(sec.name, sec.key));
    if (sec.lead && sec.lead.length) sec.lead.forEach(p => children.push(body(p)));
    else children.push(placeholder("[To be drafted. One short paragraph introducing this map.]"));
  }

  if (s.summary_stats) {
    children.push(h2("Summary statistics"));
    children.push(note("One per line, written as: number | label"));
    s.summary_stats.forEach(t => children.push(body(t)));
  }

  if (s.about) {
    children.push(h2("About These Data"));
    children.push(note(`Box title: ${s.about.title}`));
    children.push(body(s.about.body));
  }

  children.push(h2("Footnotes"));
  if (s.footnotes) s.footnotes.forEach(f => children.push(numbered(f, sheetIndex)));
  else children.push(placeholder("[To be drafted. At least one data source citation is required.]"));
}

/* ---- build ---- */
const doc = new Document({
  creator: "ADA-PARC",
  title: "ADA-PARC Fact Sheet Prose",
  description: "Shared editorial document for ADA-PARC national fact sheet prose",
  numbering: {
    config: [{
      reference: "footnote-numbers",
      levels: [{
        level: 0, format: "decimal", text: "%1.", alignment: AlignmentType.START,
        style: { paragraph: { indent: { left: 520, hanging: 300 } } }
      }]
    }]
  },
  styles: {
    default: { document: { run: { font: FONT, size: 22, color: INK } } }
  },
  sections: [{
    properties: {
      page: {
        size: { width: 12240, height: 15840 },
        margin: { top: 1300, right: 1440, bottom: 1300, left: 1440 }
      }
    },
    children
  }]
});

const out = path.join(__dirname, "..", "docs", "factsheet-prose.docx");
Packer.toBuffer(doc).then(buf => {
  fs.writeFileSync(out, buf);
  console.log("Wrote " + out);
  console.log(sheets.length + " sheets, " +
    sheets.reduce((n, s) => n + s.sections.length, 0) + " section leads");
});
