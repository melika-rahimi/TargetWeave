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

  document.addEventListener("click", function (event) {
    var button = event.target.closest("[data-password-toggle]");
    if (!button) return;
    event.preventDefault();
    togglePassword(button);
  });

  document.addEventListener("keydown", function (event) {
    if (event.key !== "Enter" && event.key !== " ") return;
    var button = event.target.closest("[data-password-toggle]");
    if (!button) return;
    event.preventDefault();
    togglePassword(button);
  });
})();
