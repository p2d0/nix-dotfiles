const fs = require("fs");
const resolve = require('path').resolve;

function searchForSln(path, depth = 0) {
  if (depth >= 10)
return false;
const files = fs.readdirSync(path)
  const resultFilename = files.find(file => {
    let filename = file.split('\\');
    filename = filename[filename.length - 1]
    if (/.+?\.sln/.test(filename)) {
      return filename;
    }
    return false;
  })
  if (!resultFilename)
    return searchForSln("../" + path, ++depth)
  return resolve(path);
}
function capitalizeFirstLetters(str) {
  const words = str.split(".");
  return words.map((item) => item.charAt(0).toUpperCase() + item.slice(1)).join(".");
}

module.exports = {
  helpers: {
    namespace: () => {
      const currentPath = resolve("./");
      const slnPath = searchForSln("./");
      const relativePath = currentPath.substring(currentPath.indexOf(slnPath) + slnPath.length + 1);
      return capitalizeFirstLetters(relativePath.replaceAll("/", "."));
    }

  }
};
