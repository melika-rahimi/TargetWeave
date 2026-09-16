(function () {
  function applyLiteratureYear(year) {
    if (!year) return;
    var wrap = document.querySelector("[data-literature-year-input]");
    var id = wrap ? wrap.getAttribute("data-literature-year-input") : null;
    if (id) {
      var select = document.getElementById(id);
      if (select && select.value !== year) {
        select.value = year;
      }
    }
    if (window.Shiny && typeof Shiny.setInputValue === "function" && id) {
      Shiny.setInputValue(id, year, {priority: "event"});
    }
  }

  document.addEventListener("click", function (event) {
    var hit = event.target.closest("[data-literature-year]");
    if (!hit) return;
    event.preventDefault();
    applyLiteratureYear(hit.getAttribute("data-literature-year"));
  });

  document.addEventListener("keydown", function (event) {
    if (event.key !== "Enter" && event.key !== " ") return;
    var hit = event.target.closest("[data-literature-year]");
    if (!hit) return;
    event.preventDefault();
    applyLiteratureYear(hit.getAttribute("data-literature-year"));
  });
})();
