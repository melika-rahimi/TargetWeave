(function () {
  function twVisibleModal() {
    return document.querySelector(".modal.show");
  }

  function twReleaseModalScrollLock() {
    if (twVisibleModal()) {
      return;
    }
    document.body.classList.remove("modal-open");
    document.body.style.removeProperty("overflow");
    document.body.style.removeProperty("padding-right");
    document.querySelectorAll(".modal-backdrop").forEach(function (el) {
      el.remove();
    });
  }

  document.addEventListener("hidden.bs.modal", function () {
    window.setTimeout(twReleaseModalScrollLock, 0);
  });

  if (window.jQuery) {
    window.jQuery(document).on("hidden.bs.modal", function () {
      window.setTimeout(twReleaseModalScrollLock, 0);
    });
  }

  document.addEventListener("click", function (event) {
    var button = event.target.closest("[data-tw-input]");
    if (!button) return;
    event.preventDefault();
    event.stopPropagation();
    if (!window.Shiny || typeof Shiny.setInputValue !== "function") return;
    var id = button.getAttribute("data-tw-input");
    var value = button.getAttribute("data-tw-value");
    if (!id) return;
    Shiny.setInputValue(id, value, {priority: "event"});
  });
})();
