// Serve docs/ the way GitHub Pages does (near enough) so I can look at it before pushing
const http = require('http')
const fs = require('fs')
const path = require('path')

const ROOT = path.resolve(__dirname, '..', 'docs')
const PORT = process.env.PORT || 8000
const TYPES = {
  '.html': 'text/html; charset=utf-8', '.css': 'text/css', '.js': 'application/javascript',
  '.xml': 'application/xml', '.json': 'application/json', '.png': 'image/png', '.jpg': 'image/jpeg',
  '.jpeg': 'image/jpeg', '.gif': 'image/gif', '.svg': 'image/svg+xml', '.ico': 'image/x-icon', '.txt': 'text/plain'
}

http.createServer((req, res) => {
  const url = decodeURIComponent(req.url.split('?')[0])
  let file = path.join(ROOT, url)
  if (!file.startsWith(ROOT)) { res.writeHead(403); return res.end() }
  if (fs.existsSync(file) && fs.statSync(file).isDirectory()) {
    if (!url.endsWith('/')) { res.writeHead(301, { Location: url + '/' }); return res.end() }
    file = path.join(file, 'index.html')
  }
  fs.readFile(file, (err, data) => {
    if (err) { res.writeHead(404, { 'Content-Type': 'text/plain' }); return res.end('404') }
    res.writeHead(200, { 'Content-Type': TYPES[path.extname(file).toLowerCase()] || 'application/octet-stream' })
    res.end(data)
  })
}).listen(PORT, () => console.log('http://localhost:' + PORT + '/'))
