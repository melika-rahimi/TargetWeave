(function () {
  function setToggleState(button, hidden) {
    button.setAttribute("aria-label", hidden ? "Show password" : "Hide password");
    button.setAttribute("title", hidden ? "Show password" : "Hide password");
    button.setAttribute("data-visible", hidden ? "false" : "true");
  }

  function togglePassword(button) {
    var id = button.getAttribute("data-password-toggle");
    if (!id) return;
    var field = document.getElementById(id);
    if (!field) return;
    var hidden = field.type === "password";
    field.type = hidden ? "text" : "password";
    setToggleState(button, !hidden);
  }

  function triggerAuthAction(wrap) {
    if (!wrap || wrap.getAttribute("data-tw-busy") === "1") return;
    var id = wrap.getAttribute("data-tw-submit");
    if (!id) return;
    if (!window.Shiny || typeof Shiny.setInputValue !== "function") return;
    wrap.setAttribute("data-tw-busy", "1");
    Shiny.setInputValue(id, Date.now(), {priority: "event"});
    window.setTimeout(function () {
      wrap.setAttribute("data-tw-busy", "0");
    }, 800);
  }

  document.addEventListener("click", function (event) {
    var button = event.target.closest("[data-password-toggle]");
    if (!button) return;
    event.preventDefault();
    togglePassword(button);
  });

  // Do not use <form>/type=submit. Shiny pages can already contain a form, so
  // the browser ignores a nested form and Enter never reaches login_submit.
  // Document-level capture survives renderUI replacement of the auth screen.
  document.addEventListener("keydown", function (event) {
    var toggle = event.target.closest("[data-password-toggle]");
    if (toggle && (event.key === "Enter" || event.key === " ")) {
      event.preventDefault();
      event.stopPropagation();
      togglePassword(toggle);
      return;
    }
    if (event.key !== "Enter" || event.repeat || event.isComposing) return;
    var field = event.target;
    if (!field || field.tagName !== "INPUT") return;
    var type = (field.getAttribute("type") || "text").toLowerCase();
    if (type === "button" || type === "submit" || type === "checkbox" || type === "radio") {
      return;
    }
    var wrap = field.closest(".auth-submit-form");
    if (!wrap) return;
    event.preventDefault();
    event.stopPropagation();
    triggerAuthAction(wrap);
  }, true);
})();
