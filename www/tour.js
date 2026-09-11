(function () {
  var overlay = null;
  var spot = null;
  var pop = null;
  var active = false;
  var steps = [];
  var index = 0;
  var mode = "tour";
  var tipName = null;
  var lastFocused = null;
  var pending = null;
  var observer = null;

  function reduceMotion() {
    return window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches;
  }

  function emit(payload) {
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue("tw_tour_event", payload, { priority: "event" });
    }
  }

  function findTarget(step) {
    if (!step) return null;
    var el = document.querySelector(step.target);
    if (!el && step.fallback) {
      el = document.querySelector(step.fallback);
    }
    return el;
  }

  function disconnectObserver() {
    if (observer) {
      observer.disconnect();
      observer = null;
    }
  }

  function clear() {
    disconnectObserver();
    pending = null;
    if (overlay) overlay.remove();
    if (spot) spot.remove();
    if (pop) pop.remove();
    overlay = spot = pop = null;
    document.removeEventListener("keydown", onKey, true);
    window.removeEventListener("resize", position);
    window.removeEventListener("scroll", position, true);
    active = false;
    if (lastFocused && lastFocused.focus) {
      try { lastFocused.focus(); } catch (e) {}
    }
    lastFocused = null;
  }

  function onKey(event) {
    if (!active) return;
    if (event.key === "Escape") {
      event.preventDefault();
      finish("skip");
    } else if (event.key === "ArrowRight") {
      event.preventDefault();
      next();
    } else if (event.key === "ArrowLeft") {
      event.preventDefault();
      back();
    } else if (event.key === "Tab" && pop) {
      var buttons = pop.querySelectorAll("button");
      if (!buttons.length) return;
      var first = buttons[0];
      var last = buttons[buttons.length - 1];
      if (event.shiftKey && document.activeElement === first) {
        event.preventDefault();
        last.focus();
      } else if (!event.shiftKey && document.activeElement === last) {
        event.preventDefault();
        first.focus();
      }
    }
  }

  function finish(reason) {
    var payload = { action: reason, kind: mode, tip: tipName, nonce: Date.now() };
    clear();
    emit(payload);
  }

  function next() {
    if (index >= steps.length - 1) {
      finish("complete");
      return;
    }
    index += 1;
    render();
  }

  function back() {
    if (index <= 0) return;
    index -= 1;
    render();
  }

  function position() {
    if (!spot || !pop) return;
    var step = steps[index];
    var el = findTarget(step);
    if (!el) return;
    var pad = 6;
    var r = el.getBoundingClientRect();
    spot.style.top = Math.max(0, r.top - pad) + "px";
    spot.style.left = Math.max(0, r.left - pad) + "px";
    spot.style.width = r.width + pad * 2 + "px";
    spot.style.height = r.height + pad * 2 + "px";

    var pw = pop.offsetWidth;
    var ph = pop.offsetHeight;
    var gap = 12;
    var left;
    var top;
    if (step.placement === "below") {
      left = r.left;
      top = r.bottom + gap;
      if (top + ph > window.innerHeight - 8) {
        top = Math.max(8, r.top - ph - gap);
      }
    } else {
      left = r.right + gap;
      top = r.top;
      if (left + pw > window.innerWidth - 8) {
        left = r.left;
        if (r.top > ph + gap + 8) {
          top = r.top - ph - gap;
        } else {
          top = r.bottom + gap;
        }
      }
    }
    left = Math.max(8, Math.min(left, window.innerWidth - pw - 8));
    top = Math.max(8, Math.min(top, window.innerHeight - ph - 8));
    pop.style.left = left + "px";
    pop.style.top = top + "px";
  }

  function advanceToAvailable() {
    while (index < steps.length && !findTarget(steps[index])) {
      index += 1;
    }
    return index < steps.length;
  }

  function remainingReady() {
    if (!pending || !pending.steps) return false;
    for (var i = index; i < pending.steps.length; i += 1) {
      if (findTarget(pending.steps[i])) return true;
    }
    return false;
  }

  function render() {
    if (!advanceToAvailable()) {
      waitForDom(remainingReady);
      return;
    }
    var step = steps[index];
    var el = findTarget(step);
    if (!el || !pop) {
      waitForDom(remainingReady);
      return;
    }
    el.scrollIntoView({
      block: "center",
      inline: "nearest",
      behavior: reduceMotion() ? "auto" : "smooth"
    });
    window.setTimeout(position, reduceMotion() ? 0 : 80);

    var total = steps.length;
    var isLast = index === total - 1;
    pop.innerHTML = "";

    var progress = document.createElement("div");
    progress.className = "tw-tour-progress";
    progress.textContent = String(index + 1).padStart(2, "0") + " / " + String(total).padStart(2, "0");

    var title = document.createElement("h2");
    title.className = "tw-tour-title";
    title.id = "tw-tour-title";
    title.textContent = step.title;

    var body = document.createElement("p");
    body.className = "tw-tour-body";
    body.id = "tw-tour-body";
    body.textContent = step.body;

    var example = null;
    if (step.example) {
      example = document.createElement("p");
      example.className = "tw-tour-example";
      example.textContent = "Example: " + step.example;
    }

    var actions = document.createElement("div");
    actions.className = "tw-tour-actions";

    var skip = document.createElement("button");
    skip.type = "button";
    skip.className = "tw-tour-skip";
    skip.textContent = mode === "setup" ? "Skip setup guide" : (mode === "tour" ? "Skip tour" : "Dismiss");
    skip.addEventListener("click", function () { finish("skip"); });
    actions.appendChild(skip);

    var nav = document.createElement("div");
    nav.className = "tw-tour-nav";

    if ((mode === "tour" || mode === "setup") && index > 0) {
      var backBtn = document.createElement("button");
      backBtn.type = "button";
      backBtn.className = "tw-tour-back";
      backBtn.textContent = "Back";
      backBtn.addEventListener("click", back);
      nav.appendChild(backBtn);
    }

    var nextBtn = document.createElement("button");
    nextBtn.type = "button";
    nextBtn.className = "tw-tour-next";
    nextBtn.textContent = isLast ? (mode === "tip" ? "Done" : "Finish") : "Next";
    nextBtn.addEventListener("click", next);
    nav.appendChild(nextBtn);

    actions.appendChild(nav);
    pop.appendChild(progress);
    pop.appendChild(title);
    pop.appendChild(body);
    if (example) pop.appendChild(example);
    pop.appendChild(actions);
    nextBtn.focus();
    position();
  }

  function ensureShell() {
    if (overlay) return;
    lastFocused = document.activeElement;
    overlay = document.createElement("div");
    overlay.className = "tw-tour-overlay" + (mode === "setup" ? " is-passive" : "");
    overlay.setAttribute("aria-hidden", "true");
    spot = document.createElement("div");
    spot.className = "tw-tour-spot";
    spot.setAttribute("aria-hidden", "true");
    pop = document.createElement("div");
    pop.className = "tw-tour-popover";
    pop.setAttribute("role", "dialog");
    pop.setAttribute("aria-modal", mode === "setup" ? "false" : "true");
    pop.setAttribute("aria-labelledby", "tw-tour-title");
    pop.setAttribute("aria-describedby", "tw-tour-body");
    document.body.appendChild(overlay);
    document.body.appendChild(spot);
    document.body.appendChild(pop);
    document.addEventListener("keydown", onKey, true);
    window.addEventListener("resize", position);
    window.addEventListener("scroll", position, true);
  }

  function waitForDom(ready) {
    if (observer || typeof ready !== "function") return;
    observer = new MutationObserver(function () {
      if (!pending) return;
      if (!ready()) return;
      disconnectObserver();
      if (active) {
        render();
      } else {
        begin(pending);
      }
    });
    observer.observe(document.body, { childList: true, subtree: true });
  }

  function begin(msg) {
    steps = msg.steps;
    index = 0;
    mode = msg.mode || "tour";
    tipName = msg.tip || null;
    active = true;
    ensureShell();
    if (overlay) {
      overlay.classList.toggle("is-passive", mode === "setup");
    }
    if (pop) {
      pop.setAttribute("aria-modal", mode === "setup" ? "false" : "true");
    }
    render();
  }

  function start(msg) {
    if (!msg || !msg.steps || !msg.steps.length) return;
    if (active && mode === "tour" && msg.mode === "tip") return;
    clear();
    pending = msg;
    // Wait for the first step's own target. Later hooks (target list)
    // are in the static UI and must not skip Project context.
    if (findTarget(msg.steps[0])) {
      begin(msg);
      return;
    }
    waitForDom(function () {
      return !!(pending && pending.steps && findTarget(pending.steps[0]));
    });
  }

  window.TWTour = {
    isActive: function () { return active; },
    start: start,
    stop: function () { clear(); }
  };

  function bindShiny() {
    if (!(window.Shiny && Shiny.addCustomMessageHandler)) {
      return false;
    }
    Shiny.addCustomMessageHandler("twTour", function (msg) {
      if (!msg || !msg.action) return;
      if (msg.action === "start" || msg.action === "tip") {
        start(msg);
      } else if (msg.action === "stop") {
        clear();
      }
    });
    return true;
  }

  if (!bindShiny()) {
    document.addEventListener("shiny:connected", bindShiny, { once: true });
  }
})();
