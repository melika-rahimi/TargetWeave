(function () {
  var WORD = "EGFR";
  var LOOP_MS = 15000;
  var TYPE_START_MS = 400;

  function prefersReduce() {
    return window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches;
  }

  function setCaption(root, step) {
    root.querySelectorAll(".demo-cap").forEach(function (cap) {
      cap.classList.toggle("is-on", cap.getAttribute("data-cap") === step);
    });
  }

  function setTabs(demo, view) {
    demo.querySelectorAll(".tw-demo-tab").forEach(function (tab) {
      tab.classList.toggle("is-on", tab.getAttribute("data-demo-tab") === view);
    });
  }

  function applyState(root, state) {
    var demo = root.querySelector(".tw-demo");
    if (!demo) {
      return;
    }
    demo.setAttribute("data-step", state.step);
    demo.setAttribute("data-view", state.view || "overview");
    if (state.resolve) {
      demo.classList.add("is-resolving");
    } else {
      demo.classList.remove("is-resolving");
    }
    if (state.saved) {
      demo.classList.add("is-saved");
    } else {
      demo.classList.remove("is-saved");
    }
    setTabs(demo, state.view || "overview");
    setCaption(root, state.step);
    var typed = demo.querySelector(".tw-demo-typed");
    if (typed && typeof state.text === "string") {
      typed.textContent = state.text;
    }
  }

  function showStatic(root) {
    root.classList.add("is-static");
    applyState(root, {
      step: "preserve",
      view: "structures",
      text: WORD,
      saved: true
    });
  }

  function Demo(root) {
    this.root = root;
    this.timers = [];
    this.letterTimer = null;
    this.loopTimer = null;
    this.running = false;
    this.paused = false;
  }

  Demo.prototype.clear = function () {
    this.timers.forEach(clearTimeout);
    this.timers = [];
    if (this.letterTimer) {
      clearTimeout(this.letterTimer);
      this.letterTimer = null;
    }
    if (this.loopTimer) {
      clearTimeout(this.loopTimer);
      this.loopTimer = null;
    }
  };

  Demo.prototype.later = function (ms, fn) {
    var self = this;
    var id = setTimeout(function () {
      fn.call(self);
    }, ms);
    this.timers.push(id);
  };

  Demo.prototype.typeWord = function (done) {
    var self = this;
    var i = 0;
    applyState(this.root, { step: "enter", view: "overview", text: "", resolve: false });
    function tick() {
      if (!self.running) {
        return;
      }
      i += 1;
      applyState(self.root, {
        step: "enter",
        view: "overview",
        text: WORD.slice(0, i),
        resolve: i >= WORD.length
      });
      if (i < WORD.length) {
        self.letterTimer = setTimeout(tick, 160);
      } else if (done) {
        done();
      }
    }
    this.later(TYPE_START_MS, tick);
  };

  Demo.prototype.runLoop = function () {
    var self = this;
    if (!this.running) {
      return;
    }
    this.clear();
    this.root.classList.remove("is-static");
    this.typeWord(function () {
      self.later(420, function () {
        applyState(self.root, { step: "identity", view: "overview", text: WORD });
      });
      self.later(2300, function () {
        applyState(self.root, { step: "disease", view: "overview", text: WORD });
      });
      self.later(3800, function () {
        applyState(self.root, { step: "evidence", view: "evidence", text: WORD });
      });
      self.later(5300, function () {
        applyState(self.root, { step: "evidence", view: "pathways", text: WORD });
      });
      self.later(6700, function () {
        applyState(self.root, { step: "evidence", view: "literature", text: WORD });
      });
      self.later(8100, function () {
        applyState(self.root, { step: "evidence", view: "structures", text: WORD });
      });
      self.later(9500, function () {
        applyState(self.root, { step: "compare", view: "overview", text: WORD });
      });
      self.later(11300, function () {
        applyState(self.root, { step: "preserve", view: "overview", text: WORD, saved: true });
      });
    });
    this.loopTimer = setTimeout(function () {
      self.root.classList.add("is-fading");
      self.later(280, function () {
        self.root.classList.remove("is-fading");
        self.runLoop();
      });
    }, LOOP_MS);
  };

  Demo.prototype.start = function () {
    if (this.running) {
      return;
    }
    this.paused = false;
    this.running = true;
    this.runLoop();
  };

  Demo.prototype.pause = function () {
    this.running = false;
    this.paused = true;
    this.clear();
  };

  Demo.prototype.stop = function (asStatic) {
    this.running = false;
    this.paused = false;
    this.clear();
    if (asStatic && this.root && this.root.isConnected) {
      showStatic(this.root);
    }
  };

  Demo.prototype.rebind = function (root) {
    this.root = root;
  };

  var demo = null;
  var observing = false;

  function syncAll() {
    var root = document.querySelector(".hero-demo");
    var reduce = prefersReduce();
    var hidden = document.hidden;

    if (reduce) {
      if (demo) {
        demo.rebind(root || demo.root);
        demo.stop(true);
      } else if (root) {
        showStatic(root);
      }
      if (root) {
        root.classList.remove("is-paused");
      }
      return;
    }

    if (!root) {
      return;
    }

    if (!demo) {
      demo = new Demo(root);
    } else {
      demo.rebind(root);
    }

    if (hidden) {
      demo.pause();
      root.classList.add("is-paused");
      return;
    }

    root.classList.remove("is-paused", "is-static");
    if (!demo.running) {
      demo.start();
    }
  }

  function watchDom() {
    if (observing || !document.body) {
      return;
    }
    observing = true;
    new MutationObserver(syncAll).observe(document.body, {
      childList: true,
      subtree: true
    });
  }

  document.addEventListener("visibilitychange", syncAll);
  if (window.matchMedia) {
    window.matchMedia("(prefers-reduced-motion: reduce)").addEventListener("change", syncAll);
  }
  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", function () {
      watchDom();
      syncAll();
    });
  } else {
    watchDom();
    syncAll();
  }
})();
