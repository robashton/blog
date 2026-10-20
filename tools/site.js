// The site builder. The Makefile decides what is out of date and calls in here.
//
//   node tools/site.js index            refresh build/index.json + build/nav/*.json (only rewritten when they change)
//   node tools/site.js posts [files..]  render the posts those changed files belong to (anything else => all posts)
//   node tools/site.js home|blog|rss|redirects
//   node tools/site.js static <src> <dest>
//   node tools/site.js new "<title>" [YYYY-MM-DD]

const fs = require('fs')
const path = require('path')
const marked = require('marked')
const hljs = require('highlight.js')
const RSS = require('rss')

const PAGES = 'input/pages'
const SOURCE = 'input/source'
const OUT = 'docs'
const BUILD = 'build'
const SITE = 'https://codeofrob.com'

marked.setOptions({
  gfm: true,
  highlight: function(code, lang) {
    try {
      return lang ? hljs.highlight(code, { language: lang }).value : hljs.highlightAuto(code).value
    } catch (e) {
      return hljs.highlightAuto(code).value
    }
  }
})

// The URL of a post is derived from its title, exactly as the original 2012 build did it.
// Don't touch this unless you fancy breaking every inbound link on the internet.
function titleToPage(title) {
  return title.toLowerCase()
    .replace(/ /g, '-')
    .replace(/[\'#\(\)]/g, '')
    .replace('/', '-')
    .replace('?', '') + '.html'
}

function titleToId(title) {
  return titleToPage(title).replace(/\.html$/, '')
    .replace(/[^a-z0-9.+-]+/g, '-').replace(/^-+|-+$/g, '')
}

function read(file) { return fs.readFileSync(file, 'utf8') }
function readJson(file) { return JSON.parse(read(file)) }

function writeIfChanged(file, content) {
  fs.mkdirSync(path.dirname(file), { recursive: true })
  if (fs.existsSync(file) && read(file) === content) return false
  fs.writeFileSync(file, content, 'utf8')
  return true
}

function escapeHtml(s) {
  return String(s).replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;').replace(/"/g, '&quot;')
}

const MONTHS = ['January', 'February', 'March', 'April', 'May', 'June', 'July',
                'August', 'September', 'October', 'November', 'December']

function longDate(iso) {
  const d = new Date(iso)
  return d.getUTCDate() + ' ' + MONTHS[d.getUTCMonth()] + ' ' + d.getUTCFullYear()
}

function shortDate(iso) { return new Date(iso).toISOString().slice(0, 10) }

// Dumb {{name}} substitution, unknown keys are left alone so they're easy to spot
function fill(template, values) {
  return template.replace(/\{\{(\w+)\}\}/g, (m, k) => (k in values ? values[k] : m))
}

function layout(values) {
  return fill(read(path.join(SOURCE, 'layout.html')), Object.assign({
    description: "Rob Ashton's blog. Erlang, Purescript, live video, and whatever else I've been getting annoyed about.",
    head: '',
    ogtype: 'website',
    ogtitle: values.title,
    canonical: escapeHtml(SITE + (values.url || '/')),
    year: new Date().getUTCFullYear()
  }, values))
}

// First bit of prose in a post, for the description/link previews
function excerpt(html) {
  const text = html.replace(/<(script|style|pre)[\s\S]*?<\/\1>/gi, ' ').replace(/<[^>]*>/g, ' ')
    .replace(/&nbsp;/g, ' ').replace(/&amp;/g, '&').replace(/&quot;/g, '"').replace(/&#39;/g, "'")
    .replace(/&lt;/g, '<').replace(/&gt;/g, '>').replace(/\s+/g, ' ').trim()
  return escapeHtml(text.length > 200 ? text.slice(0, 200).replace(/\s+\S*$/, '') + '...' : text)
}

// ---------------------------------------------------------------------------
// The index: every published post, oldest first

function scanPosts() {
  const now = new Date()
  const posts = []
  for (const id of fs.readdirSync(PAGES)) {
    const metaFile = path.join(PAGES, id, 'meta.json')
    if (!fs.existsSync(metaFile)) continue
    const meta = readJson(metaFile)
    const date = new Date(meta.date)
    if (date > now) continue // queued for the future, it'll appear on the first build after its date
    posts.push({
      id: id,
      title: meta.title,
      date: date.toISOString(),
      url: '/entries/' + titleToPage(meta.title),
      redirect: /^\/archive\//.test(meta.redirect || '') ? meta.redirect : null
    })
  }
  return posts.sort((a, b) => (a.date < b.date ? -1 : a.date > b.date ? 1 : 0))
}

function loadIndex() { return readJson(path.join(BUILD, 'index.json')) }

function cmdIndex() {
  const posts = scanPosts()
  writeIfChanged(path.join(BUILD, 'index.json'), JSON.stringify(posts, null, 1))

  // One prev/next file per post, so adding a post only re-renders its neighbour
  const navDir = path.join(BUILD, 'nav')
  fs.mkdirSync(navDir, { recursive: true })
  const live = new Set()
  posts.forEach((p, i) => {
    const pick = q => (q ? { title: q.title, url: q.url } : null)
    live.add(p.id + '.json')
    writeIfChanged(path.join(navDir, p.id + '.json'),
      JSON.stringify({ older: pick(posts[i - 1]), newer: pick(posts[i + 1]) }))
  })
  for (const f of fs.readdirSync(navDir)) if (!live.has(f)) fs.unlinkSync(path.join(navDir, f))

  // Anything in docs/entries we didn't produce is either a renamed/deleted post or something
  // somebody (me) dropped in by hand, either way worth knowing about
  const expected = new Set(posts.map(p => path.basename(p.url)))
  const strays = fs.readdirSync(path.join(OUT, 'entries')).filter(f => !expected.has(f))
  if (strays.length) console.error('warning: docs/entries has files with no published post:\n  ' + strays.join('\n  '))
}

// ---------------------------------------------------------------------------
// Posts

let redirectMap = null
let redirectByDate = null
function fixLinks(html, index) {
  if (!redirectMap) {
    redirectMap = new Map()
    redirectByDate = new Map()
    for (const p of index) {
      if (!p.redirect) continue
      redirectMap.set(p.redirect.toLowerCase(), p.url)
      const day = p.redirect.slice(0, '/archive/yyyy/mm/dd/'.length)
      redirectByDate.set(day, redirectByDate.has(day) ? null : p.url) // null = more than one post that day
    }
  }
  // Posts from the Community Server days link to each other via /archive/yyyy/mm/dd/*.aspx, and the
  // slugs in those links don't always match the ones I kept, so fall back to the date if it's unambiguous
  const resolve = p => redirectMap.get(p.toLowerCase()) || redirectByDate.get(p.slice(0, '/archive/yyyy/mm/dd/'.length))
  return html
    .replace(/href="(?:https?:\/\/(?:www\.|blog\.)?codeofrob\.com)?(\/archive\/\d{4}\/\d\d\/\d\d\/[^"]+?\.aspx)\s*"/gi,
      (m, p) => (resolve(p) ? 'href="' + resolve(p) + '"' : m))
    .replace(/(href|src)="https?:\/\/(?:www\.)?codeofrob\.com\//gi, '$1="/')
}

function postBody(id) {
  const md = path.join(PAGES, id, 'content.markdown')
  if (fs.existsSync(md)) return marked(read(md))
  // Windows Live Writer left empty code blocks lying around as spacers
  return read(path.join(PAGES, id, 'content.html'))
    .replace(/<div class="csharpcode">(\s|&nbsp;)*<\/div>/g, '')
}

function renderComments(id) {
  const file = path.join(PAGES, id, 'comments.json')
  const comments = fs.existsSync(file) ? readJson(file) : []
  if (!comments.length) return ''
  const items = comments.map(c =>
    '<li><p class="comment-author">' + escapeHtml(c.author.name) +
    (c.date ? ' <span>' + escapeHtml(String(c.date).replace(/[()]/g, '')) + '</span>' : '') + '</p>' +
    '<div class="comment-body">' + escapeHtml(c.text.trim()).replace(/\r?\n/g, '<br>') + '</div></li>').join('\n')
  const n = comments.length
  return '<details class="comments"><summary>' + n + ' comment' + (n === 1 ? '' : 's') +
    ' from the old blog</summary><ol>' + items + '</ol></details>'
}

function renderNav(id) {
  const nav = readJson(path.join(BUILD, 'nav', id + '.json'))
  const link = (p, rel, label) => (p ? '<a rel="' + rel + '" href="' + p.url + '"><span>' + label + '</span>' + escapeHtml(p.title) + '</a>' : '<span></span>')
  return link(nav.older, 'prev', 'Older') + link(nav.newer, 'next', 'Newer')
}

function renderPost(post, index) {
  const body = fixLinks(postBody(post.id), index)
  const html = fill(read(path.join(SOURCE, 'post.html')), {
    title: escapeHtml(post.title),
    isodate: shortDate(post.date),
    date: longDate(post.date),
    body: body,
    comments: renderComments(post.id),
    nav: renderNav(post.id)
  })
  fs.writeFileSync(path.join(OUT, post.url), layout({
    title: escapeHtml(post.title) + ' - Rob Ashton', ogtitle: escapeHtml(post.title), ogtype: 'article',
    description: excerpt(body), url: encodeURI(post.url), content: html
  }), 'utf8')
}

function cmdPosts(files) {
  const index = loadIndex()
  let ids = null
  if (files.length) {
    ids = new Set()
    for (const f of files) {
      const m = f.match(/^input\/pages\/([^/]+)\//) || f.match(/^build\/nav\/(.+)\.json$/)
      if (!m) { ids = null; break } // a template or the builder itself changed, so everything goes
      ids.add(m[1])
    }
  }
  const todo = index.filter(p => !ids || ids.has(p.id))
  todo.forEach(p => renderPost(p, index))
  console.log('rendered ' + todo.length + ' post' + (todo.length === 1 ? '' : 's'))
}

// ---------------------------------------------------------------------------
// Listing pages

function postList(posts) {
  return '<ul class="posts">\n' + posts.map(p =>
    '<li><time datetime="' + shortDate(p.date) + '">' + shortDate(p.date) + '</time> <a href="' + p.url + '">' + escapeHtml(p.title) + '</a></li>').join('\n') + '\n</ul>'
}

function cmdHome() {
  const index = loadIndex()
  const recent = index.slice(-8).reverse()
  const content = fill(read(path.join(SOURCE, 'home.html')), { recent: postList(recent) })
  fs.writeFileSync(path.join(OUT, 'index.html'), layout({ title: 'Rob Ashton', url: '/', description: excerpt((content.match(/<p>[\s\S]*?<\/p>/) || [''])[0]), content: content }), 'utf8')
}

function cmdBlog() {
  const index = loadIndex()
  const years = {}
  for (const p of index.slice().reverse()) (years[p.date.slice(0, 4)] = years[p.date.slice(0, 4)] || []).push(p)
  const byYear = Object.keys(years).sort().reverse().map(y =>
    '<section class="year"><h3 id="y' + y + '">' + y + '</h3>\n' + postList(years[y]) + '</section>').join('\n')
  const content = fill(read(path.join(SOURCE, 'blog.html')), { byyear: byYear, count: index.length })
  fs.writeFileSync(path.join(OUT, 'blog.html'), layout({ title: 'Writing - Rob Ashton', url: '/blog.html', content: content }), 'utf8')
}

function cmdRss() {
  const index = loadIndex()
  const feed = new RSS({
    title: "Rob Ashton's blog",
    description: 'Software development dumping ground',
    feed_url: SITE + '/rss.xml',
    site_url: SITE,
    image_url: SITE + '/img/cover.jpg',
    author: 'Rob Ashton'
  })
  const latest = index.slice(-10).reverse()
  for (const p of latest) {
    feed.item({
      title: p.title,
      url: SITE + p.url,
      guid: 'http://codeofrob.com' + p.url, // what the old feed used, so readers don't think every post is new
      description: fixLinks(postBody(p.id), index).replace(/<!\[CDATA\[/g, '').replace(/\]\]>/g, ''),
      date: p.date
    })
  }
  // The rss module stamps lastBuildDate with "now", pin it to the newest post so the output is stable
  const xml = feed.xml().replace(/<lastBuildDate>[^<]*<\/lastBuildDate>/,
    '<lastBuildDate>' + new Date(latest.length ? latest[0].date : 0).toUTCString() + '</lastBuildDate>')
  writeIfChanged(path.join(OUT, 'rss.xml'), xml)
}

// Old /archive/yyyy/mm/dd/slug.aspx URLs: GitHub Pages can't do redirects, but it will
// happily serve slug.aspx/index.html, so drop a forwarding page in at each one
function cmdRedirects() {
  const index = loadIndex()
  let n = 0
  for (const p of index) {
    if (!p.redirect) continue
    const html = '<!doctype html><meta charset="utf-8"><title>' + escapeHtml(p.title) + '</title>' +
      '<link rel="canonical" href="' + SITE + p.url + '"><meta http-equiv="refresh" content="0; url=' + p.url + '">' +
      '<p>This post has moved to <a href="' + p.url + '">' + p.url + '</a>.</p>\n'
    if (writeIfChanged(path.join(OUT, p.redirect, 'index.html'), html)) n++
  }
  console.log('wrote ' + n + ' redirect page' + (n === 1 ? '' : 's'))
}

function cmdSitemap() {
  const index = loadIndex()
  const statics = fs.readdirSync('input/static').map(f => '/' + f.replace(/\.(markdown|md)$/, '.html'))
  const newest = index.length ? index[index.length - 1].date.slice(0, 10) : ''
  const urls = [['/', newest], ['/blog.html', newest]].concat(statics.map(u => [u, '']))
    .concat(index.map(p => [encodeURI(p.url), p.date.slice(0, 10)]))
  const xml = '<?xml version="1.0" encoding="UTF-8"?>\n<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">\n' +
    urls.map(([u, d]) => '<url><loc>' + escapeHtml(SITE + u) + '</loc>' + (d ? '<lastmod>' + d + '</lastmod>' : '') + '</url>').join('\n') +
    '\n</urlset>\n'
  writeIfChanged(path.join(OUT, 'sitemap.xml'), xml)
}

function cmdStatic(src, dest) {
  let body = read(src)
  if (/\.(markdown|md)$/.test(src)) body = marked(body)
  const h1 = body.match(/<h1[^>]*>([\s\S]*?)<\/h1>/)
  const title = h1 ? h1[1].replace(/<[^>]*>/g, '').trim() + ' - Rob Ashton' : 'Rob Ashton'
  fs.writeFileSync(dest, layout({ title: title, url: '/' + path.basename(dest), content: '<article class="page">\n' + body + '\n</article>' }), 'utf8')
}

// ---------------------------------------------------------------------------

function cmdNew(title, dateStr) {
  if (!title) throw new Error('usage: make new TITLE="Some title" [DATE=yyyy-mm-dd]')
  const id = titleToId(title)
  const dir = path.join(PAGES, id)
  if (fs.existsSync(dir)) throw new Error(dir + ' already exists')
  const date = dateStr ? new Date(dateStr + 'T09:30:00Z') : new Date()
  fs.mkdirSync(dir)
  fs.writeFileSync(path.join(dir, 'meta.json'), JSON.stringify({ title: title, date: date.toISOString() }) + '\n')
  fs.writeFileSync(path.join(dir, 'comments.json'), '[]\n')
  fs.writeFileSync(path.join(dir, 'content.markdown'), '')
  console.log(dir + '/content.markdown, publishing ' + date.toISOString() + ' at ' + SITE + '/entries/' + titleToPage(title))
}

const [cmd, ...args] = process.argv.slice(2)
const commands = {
  index: cmdIndex, posts: () => cmdPosts(args), home: cmdHome, blog: cmdBlog, rss: cmdRss,
  redirects: cmdRedirects, sitemap: cmdSitemap, static: () => cmdStatic(args[0], args[1]), new: () => cmdNew(args[0], args[1])
}
if (!commands[cmd]) {
  console.error('unknown command: ' + cmd)
  process.exit(1)
}
commands[cmd]()
