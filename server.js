const http = require("http");
const fs = require("fs");
const url = require("url");
const path = require("path");

const sseScript = `
  const es = new EventSource("http://localhost:3000/sse");
  es.onmessage = function (e) {
  const op = JSON.parse(e.data);
  location.hash = op.slide;
  op.reload && location.reload();
};
`;

const slideFile = process.argv[2] + ".slide";

const readSlide = () =>
  fs
    .readFileSync(slideFile)
    .toString()
    .replace(/\n/g, "");

const sseMsg = data => `data: ${JSON.stringify(data)}\n\n`;

http
  .createServer((req, res) => {
    if (req.url == "/sse") {
      res.writeHead(200, {
        connection: "keep-alive",
        "cache-control": "no-cache",
        "content-type": "text/event-stream"
      });

      fs.watchFile(slideFile, { interval: 100 }, _ => {
        const rawSlide = readSlide();
        const slide = rawSlide.replace("reload", "");
        if (rawSlide != null) {
          res.write(
            sseMsg({
              slide,
              reload: rawSlide.indexOf("reload") != -1
            })
          );
        }
      });
      req.on("close", () => fs.unwatchFile(slideFile));

      return res.write(
        sseMsg({
          slide: readSlide().replace("reload", "")
        })
      );
    }

    const parsedUrl = url.parse(req.url);
    let pathname = `.${parsedUrl.pathname}`;
    const ext = path.parse(pathname).ext || ".html";
    const map = {
      ".ico": "image/x-icon",
      ".html": "text/html",
      ".js": "text/javascript",
      ".json": "application/json",
      ".css": "text/css",
      ".png": "image/png",
      ".jpg": "image/jpeg",
      ".wav": "audio/wav",
      ".mp3": "audio/mpeg",
      ".svg": "image/svg+xml",
      ".pdf": "application/pdf",
      ".doc": "application/msword"
    };

    fs.exists(pathname, function(exist) {
      if (!exist) {
        res.writeHead(404);
        res.end(`File ${pathname} not found!`);
        return;
      }

      if (fs.statSync(pathname).isDirectory()) {
        pathname += "/index" + ext;
      }

      fs.readFile(pathname, function(err, data) {
        if (err) {
          res.writeHead(500);
          res.end(`Error getting the file: ${err}.`);
        } else {
          res.setHeader("content-type", map[ext] || "text/plain");
          res.end(
            pathname.indexOf("index.html") >= 0
              ? data
                  .toString()
                  .replace("</body>", `<script>${sseScript}</script></body>`)
              : data
          );
        }
      });
    });
  })
  .listen(3000);
