const fs = require("fs/promises");
const path = require("path");
module.exports = (req, res, next) => {
  if (!["GET", "HEAD"].includes(req.method) || path.extname(req.url))
    return next();

  fs.access(path.join("_site", req.url) + ".html")
    .then(() => req.url += ".html", () => {})
    .finally(() => next());
}
