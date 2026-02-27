(function() {
    function updateAttr() {
        if (typeof gBrowser !== 'undefined') {
            document.documentElement.setAttribute("currenturl", gBrowser.currentURI.spec);
        }
    }
    window.addEventListener("TabSelect", updateAttr);
    window.addEventListener("DOMContentLoaded", updateAttr);
    setTimeout(updateAttr, 1000);
})();
