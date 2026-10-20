# codeofrob.com refresh — survey + proposal (2026-10-20)

Status: **proposal only, nothing changed in the site yet.** Waiting on Rob's answers
to the questions at the bottom before writing any copy into the site.

Input: ChatGPT handover (pasted into session), plus Rob's addition: don't take credit
for the team's work. Studio was his off his own back; Playout and Commentary he is
"largely responsible for". Tread carefully.

`ai/` is at the repo root, which is outside `docs/` (the GitHub Pages root), so these
notes never get published.

---

## 1. What exists

### Tech

- Hand-rolled static generator from 2012 (`build.js`, `common.js`). Each post is
  `input/pages/<Title>/{meta.json,content.markdown|content.html,comments.json}`.
  `marked` 0.7 + highlight.js 10 + `plates` templating. Output is committed to `docs/`.
- Hosted on GitHub Pages from `docs/` (since May 2023), CNAME `codeofrob.com`.
- `.github/workflows/run-on-schedule.yml` rebuilds on push and every Monday 16:00,
  then commits `docs/`. That's where the ~700 "Add periodic changes" commits come from.
  Usually the only change is RSS `lastBuildDate`. The point of the cron is that
  future-dated posts publish themselves (`newpost.js` queues posts on Tue/Thu 09:30).
- `server.js` (paperboy + 301s from old `/archive/YYYY/MM/DD/*.aspx` URLs) is from
  the Heroku days. **It doesn't run on GH Pages**, so the old .aspx URLs from 2009–11
  404 now (checked).
- **Builds fine on Node 22 in ~3s** and reproduces the committed `docs/` (only the
  RSS date and a timezone-sensitive date differ). Tested in a scratch clone.
- CI uses `actions/setup-node@v1` with Node 12 and `actions/checkout@v2`. It still
  ran last night, but it's all deprecated and will break eventually.
- `docs/` is 71MB, mostly old images and the Bootstrap 2 "Unify" theme assets.

### Pages / nav

- `/` (`docs/index.html`) is a full-screen iframe of
  `robashton.github.io/crowdsourcedhomepage`: the PR-driven joke homepage (ponies,
  "MICROSOFT ANGUALR MVP FTW", "UNDER CONSTRUCTION", nyan cat). It's the first thing
  anyone sees.
- `/blog.html` shows the 10 most recent posts plus a **hand-curated series list in
  `input/source/indextemplate.html` that stops in about 2014** (EventStore, frameworkless
  JS, Kotlin, Clojure space invaders, SOLID, Cascalog/OHD, agile/work-life, Hibernating
  Rhinos/RavenDB/LevelDB, Opinions, Project write-ups, OMeta/OData). None of the 2016–2023
  work is in it.
- There's no full archive page. The only full list is `docs/allposts.txt`.
- Static pages (`input/static/` → `docs/`): `projects.html` (CravenDB, RavenDB,
  HoverBattles, Ludum Dare games, around 2010–13), `speaking.html` (Øredev keynote,
  DevDay, NDC 2012, roughly up to 2015), `statements.html` (proper testimonials:
  Ayende, Rulemotion, Truthvine...), `testimonials.html` ("Nice things people have
  said about me": "Typical cowboy bullshit", "Blunt. Pain in the ass"), and
  `hire.html` ("I am not really hireable at the moment"). Only Home and Blog are in
  the nav.
- Post template: header and nav, title, "Published on", body, then Index / Subscribe /
  Respond links. Footer says "2020 © Rob Ashton".

### Content

261 posts: 2009 (4), 2010 (49), 2011 (18), 2012 (12), **2013 (118)**, 2014 (16),
2015 (4), 2016 (5), 2018 (6), 2019 (8), 2020 (12), 2021 (8), 2023 (1).

The last decade isn't actually missing from the site, it just isn't surfaced:

- Purescript on the BEAM (2018, 6 posts)
- Pinto/Stetson intro series (2019, 6 posts), plus the CodeMesh 2019 talk and the Q&A post
- Purerl Updates (2020, 6 posts; 2021, 6 posts)
- Blogging the mundane / NVENC (2020, 5 posts)
- "The launching of a product, or where I've been for the last few years" (2023):
  the id3as → Norsk story in Rob's own words. **This is the best existing source
  for the new copy.**

### Orphaned pages that are live right now

These are in `docs/entries/` with no source in `input/pages/` and no index link, but
they're served at their URLs (checked with curl):

- `an-adventure-in-llm+agent-development.html` (2025-09-18) and
  `llm-integration---designing-llm-tools.html` / `---design-time-chat.html`: real posts
  about Studio's LLM integration. Source deleted in commit 734fb708 ("fuck it, can't be
  arsed writing LLM posts"), HTML kept in 26823c1e ("for posterity").
- `llm-integration---{a-basic-agent-loop,a-basic-execution-loop,agentic-prompt-chaining,keeping-the-user-updated,prompt-chaining-to-reduce-llm-confusion,pruning+managing-history,tool-usage}.html`.
  **Several of these are literally the `[title](url) ![alt](img)` newpost stub,
  published.**
- `purerl---three-years-on.html` (2021), `using-nvenc-to-encode-h264.html`,
  `escaping-an-adt-explosion-with-data.variant-and-row-types-in-purescript.html`:
  older drafts or renames.

The 2025 LLM posts describe Studio internals and possible future features ("AI
Operators", "magic fix-this button"). Some of that may have shipped and been announced
since (the norsk.video blog has "Norsk Studio MCP" posts). **Rob to decide: restore,
finish, or unpublish.** The empty stubs should go regardless.

### Visual identity

Bootstrap 2 "Unify" theme. Bold sans name top-left, uppercase nav, green breadcrumb
band on the blog index, then a brighter green "Past entries" band. Post body is a
serif at full container width (about 1140px lines on desktop, too long to read
comfortably). Post pages put a ~110px left indent on mobile, which wastes a third of
a 390px screen. gruvbox-light code highlighting. Overall it's plain and slightly
dated, and fine. Nothing about it says "portfolio" or "startup", and that's worth
keeping.

### Dead or broken bits

- Subscribe link and `<link rel=alternate>` point at `feed.codeofrob.com/RobAshton`,
  which 404s. `/rss.xml` works.
- Google Fonts are loaded over `http://`, so they're blocked as mixed content.
- `ga.js` Universal Analytics was switched off by Google in 2023, so it's dead weight.
- Footer "2020 ©".
- Old .aspx redirects (see above).
- RSS description: "Software development dumping ground" (keep it, honestly).

---

## 2. Worth preserving (all of it, basically)

- Every `/entries/*.html` URL, date, code block and comment. Nothing gets renamed.
- The generator. It's adequate, it's fast, and replatforming would mean re-checking
  261 posts' rendering and URLs for no gain. **Recommendation: no replatform.**
- The look, more or less: plain header, serif body, gruvbox code.
- `testimonials.html` (the insults page). It's the most "Code of Rob" thing on the
  site.
- The crowdsourced homepage, as a thing that exists. Just not as the front door
  (see below).
- The README ("It's 2018 and it's still a bloody mess").

## 3. What's outdated or missing

- The front door is a 2013 joke and says nothing about who Rob is now.
- Nothing anywhere says "I work at id3as/Norsk and built Studio". The 2023 post
  comes closest but it's one post deep.
- The series list stops at 2014, so the Purerl/Pinto/Stetson/NVENC work, which is the
  most relevant evidence of the last decade, isn't visible.
- `projects.html` and `speaking.html` stop around 2013–15.
- `hire.html` is from the freelance era. It isn't linked from the nav, but it's
  indexed.

## 4. Proposed restrained update

Same generator, same CSS, same URLs. Changes:

1. **New `/` homepage** built through the static template so it gets the normal
   header/nav: a few plain paragraphs (who, where, what I've built, with links),
   then recent posts, then the series. No hero, no tagline.
2. **Move the crowdsourced iframe to `/crowdsourced.html`** and link it from the
   homepage or footer ("the old homepage, which the internet wrote for me").
3. **`/blog.html`:** add the missing series (Purescript on the BEAM, Pinto+Stetson,
   Purerl Updates 2020/2021, Blogging the mundane/NVIDIA) to the curated list, and add a
   link to the full archive.
4. **New generated `/archive.html`:** every post grouped by year (about 15 lines in
   `build.js`).
5. **`projects.html` → add a "Since 2014: Norsk" section at the top** (Studio,
   Playout, Remote Commentary, plus Pinto/Stetson/purerl as open source) and keep the
   old projects below under "Older things". Same URL, no agency-style case studies.
6. **`speaking.html`:** add CodeMesh 2019 (Purerl/OTP) and the 2024 Jan Ozer
   "Norsk Studio: A Developer's Deep Dive" (norsk.video/blog/norsk-studio-a-developers-deep-dive,
   YouTube lLLc-JlJVKM).
7. **Nav:** Home / Blog / Projects / Speaking. Testimonials linked from the footer.
   `hire.html` stays at its URL but gets unlinked, and perhaps rewritten to one line
   ("Not looking, thanks, but email me if you want to talk about media stuff").
8. **Housekeeping:** fix the feed links to `/rss.xml`, https fonts, drop `ga.js`,
   computed footer year, CSS `max-width: ~70ch` on `#post` and remove the mobile
   left indent, CI to Node 22 / current actions, and only commit when something other
   than `lastBuildDate` changes (optional, but it kills the noise commits).
9. **Optional:** static redirect stubs for the old `.aspx` URLs, generated from
   `meta.redirect`. Only worth doing if old inbound links still matter.
10. **Orphaned LLM pages:** per Rob's decision. Delete the empty stubs at minimum.

Explicitly **not** proposed: a new SSG, tags/categories infrastructure, a "series"
metadata system (the hand-curated list is fine and matches how Rob writes), dark-mode
redesign, or comments.

## 5. Voice notes (from reading ~10 posts)

Read: Blogging the mundane, Your container is not wanted here, Uncle Bob's viewpoint
considered harmful, Getting started with NVEnc, Purescript on the BEAM intro, Why
you can't be a good .NET developer, The launching of a product (2023), An adventure in
LLM+Agent development (2025), Designing LLM Tools (2025), plus the static pages.

- Long, run-on sentences with parenthetical asides: "(is this a word?)", "(*and I do
  by the way*)", "(Yes, that's literally all of our jobs)".
- Spaced hyphen ` - ` as the dash, never em dashes.
- Bullet lists mid-argument, often joke lists ("A) YAML B) Kubernetes C) YAML and
  Kubernetes").
- Bold or italic pseudo-headings and short `==` headings ("Enter Norsk", "Yeah... no.").
- Self-deprecating about his own output ("terribly written JS that hasn't changed in
  nearly a decade - who says code rots?") and openly sceptical: **"I'm an AI sceptic"**
  is the first line of the 2025 post, and LLMs are "kinda shit".
- British, mild swearing, conversational asides to the reader. Typos survive.
- He describes products by what they *do*, concretely ("every component automatically
  presents swagger/http control surface, UIs around that control surface,
  metrics/alerts"). He never uses labels about himself.
- Existing phrases worth reusing: Studio was "originally created in a weekend with a
  goal of 'Windows forms for media workflows'"; "productising (is this a word?) the
  knowledge and code we've built up"; "it's like being able to run ffmpeg and change
  everything about anything while it's running".

## 6. Draft homepage copy (for review, NOT applied)

Kept short and plain. Uses Rob's own phrasing where it exists. `[CONFIRM]` marks
facts I don't have.

> I'm Rob Ashton. I write software, and for a while there I wrote quite a lot
> *about* software on this site, mostly in long series about whatever I was
> learning at the time, with the occasional rant about something I thought was
> rubbish.
>
> Since [CONFIRM: year] I've worked at [id3as](https://www.id3as.com/), which these
> days mostly goes by the name of its product, [Norsk](https://norsk.video/). Norsk is
> a programmable live media server and SDK. The team has been building it out of
> fifteen-odd years of broadcast and streaming work (Erlang, Purescript, Rust, C,
> CUDA, and whatever else the job needed). For the last few years my part of that has
> mostly been building the products on top of it:
>
> - [Norsk Studio](https://norsk.video/norsk-studio-live-media-workflow-server/):
>   I wrote the first version in a weekend with the goal of "Windows Forms for media
>   workflows" and have kept building it since. It's a drag and drop workflow builder
>   where every component brings its own HTTP API, UI and metrics with it.
> - [Norsk Playout](https://norsk.video/norsk-playout) and
>   [Norsk Remote Commentary](https://norsk.video/norsk-remote-commentary): built on
>   Studio, and I've done most of the work on both [CONFIRM wording].
>
> None of that would be much use without the engine underneath, which is very much
> a team effort.
>
> The old writing is all still here: [the blog](/blog.html), or
> [everything, by year](/archive.html). There's also [a list of things I've built](/projects.html),
> some [talks](/speaking.html), and [the old homepage](/crowdsourced.html), which the
> internet wrote for me.

Things deliberately left out of the draft: the 100-person org (see questions), AI,
job titles, Turnkey, the cloud/SaaS line, and anything commercial.

## 7. Where I'd push back on the ChatGPT handover

- **The AI framing is ChatGPT's thesis, not obviously Rob's.** His most recent
  writing opens with "I'm an AI sceptic", and he abandoned the LLM series out of
  boredom. The "AI amplifies experienced engineers" essay might well be worth writing,
  but it should come out of what he actually thinks. It shouldn't be positioning on
  the homepage, and the draft has none.
- **Descriptive labels** ("product engineer", "technical product architect"): his
  writing never labels himself, it just says what he did. I'd use no label at all.
- **"At Norsk for roughly a decade"**: for most of that it was id3as, and Norsk is
  the product the company is now named for. Saying that accurately is also the
  easiest way to make clear he didn't found it.
- **The decade isn't invisible, it's unindexed.** There are 40-odd posts from
  2018–2021 about Purescript on the BEAM, Pinto/Stetson and NVENC. Surfacing those is
  cheaper and more convincing than new copy.
- **The "bespoke on a platform" story is partly public already.** Norsk Turnkey was
  announced for IBC 2026 ("Norsk Studio engineers build custom live video workflows to
  a customer's exact specifications"). It's fine to reference, but the essay idea
  needs care around customers and commercials.
- **100-person engineering org "about twelve years ago"**: that would be about 2014,
  but 2013 on this blog is the journeyman/working-for-free year. Needs checking, and
  it may belong on LinkedIn rather than here.

## 8. Open questions for Rob

1. Year he started at id3as, and how he'd describe the company (id3as / Norsk / both).
2. Exact wording he's comfortable with for Playout and Commentary, and whether Probe,
   Manager, CTL or Relay (all public on norsk.video) involve enough of his work to list.
3. Homepage: move the crowdsourced page to `/crowdsourced.html` (my recommendation),
   keep it as `/`, or retire it?
4. Orphaned 2025 LLM posts: restore (OK with Norsk?), finish, or unpublish? Empty stubs
   get deleted either way?
5. `hire.html`: rewrite to one line, leave as-is, or remove?
6. The 100-person org: what and when, and does he want it on the site at all?
7. Bother with .aspx redirect stubs?

---

## 9. Rob's answers (2026-10-20) and what got done

Answers:
- Started at id3as in 2015 (11 years, going on 12). Say that id3as became Norsk. Don't say "at Norsk for a decade".
- Products: don't claim Playout/Commentary. Say he built Studio and that it underpins the product range.
- The 100-person org was a technical-leader role (around 100 people) inside a 38,000-person consultancy. Public
  wording: "Technical Expert at some of the largest strategic consultancies in the world". Nothing more.
- Speaking: "used to speak at conferences before the start-up life took me".
- Crowdsourced homepage: keep it and link to it, but retire it from `/`.
- Kill the LLM pages and hire.html.
- "Fix and/or redesign everything". Replace the build with a Makefile that only regenerates what's out
  of date, with nothing running on the server.

Done (uncommitted, staged in the working tree):
- Hosting is GitHub Pages from `docs/` on main (CNAME). The Action, server.js, Procfile and commit
  script are all deleted. Build is `make` inside `nix develop` (new flake: node 22 + gnumake;
  old env/ deleted). Added `docs/.nojekyll`.
- `tools/site.js` plus a `Makefile`. `$(shell site.js index)` refreshes build/index.json and per-post
  build/nav/<id>.json, writing only when content changes. `build/posts.stamp` passes `$?` so only
  changed posts get re-rendered (a new post re-renders itself and its older neighbour). Full build is ~4.5s
  and an idle `make` does nothing. `make rebuild` reproduces docs/ byte for byte.
- Post folders renamed to safe slugs (git mv). **URLs still derive from meta.title via the original
  titleToPage.** The 184 cached content.html files beside markdown were removed. 77 legacy HTML-only posts remain.
- Deps trimmed to marked 0.7.0 / highlight.js 10.7.1 / rss 1.2.2, pinned to what the old build used
  so rendering doesn't change.
- Future-dated posts are no longer emitted at all (previously they were live by URL before their date).
  Queued posts appear on the first `make` after their date, since there's no cron any more.
- RSS: https URLs, guid kept as the old http URL (so readers don't re-notify), lastBuildDate pinned to
  the newest post. Feed link fixed to /rss.xml (feed.codeofrob.com 404s).
- 72 redirect stubs at docs/archive/.../slug.aspx/index.html (Pages serves the dir index). Internal
  .aspx links in old posts are rewritten at build time, falling back to the date when the slug differs.
  Fixed 3 typo'd internal links in sources (.htm, stray quote). One remaining dead internal link
  (ravendb-basic-usage-considerations.aspx, never migrated).
- Redesign: new layout/post/home/blog templates, a single docs/css/site.css (Source Serif 4 + Source
  Code Pro, 42rem measure, light/dark, gruvbox highlight in both, overrides for Windows Live Writer
  code blocks, empty WLW spacer divs stripped). Old posts' comments are kept in a collapsed <details>.
- Pages: new homepage, blog.html (5 new series plus the 12 old ones, then everything by year),
  projects/speaking/statements/testimonials rewritten (dead Heroku game links dropped), and
  crowdsourced.html (the old iframe homepage).
- Deleted: LLM orphans, empty stubs, the unlinked ADT draft page (source still in scratch/), hire,
  available, oldindex, references (dupe), docs/assets theme (18MB), old css/js, allposts.txt, tags (17MB ctags).
- Pre-existing, not fixed: the second chart on the "sweariest commit messages" post has been broken on
  the live site for years (JS error). a-rob-update links to /hire.html, which is now a 404.

Still to confirm with Rob: the homepage copy as a whole; "then a freelancer" ordering; Pinto/Stetson
wording; the jokes in the projects copy ("most starred thing I own", "Cookbook, which nobody reads").

## 10. Shipped

Pushed as 1d906751 on 2026-10-20 and live on codeofrob.com within about 30s. Checked live: pages, odd-character
URLs, the restored 2010 post, and the .aspx redirects (301 to slug.aspx/, then a meta refresh). hire.html returns 404 as intended.
Afterwards, Rob changed the homepage framing to "I design and build products" (ChatGPT's point about not leading
with "programmer"). Next step: Rob gets ChatGPT to review the live site.
