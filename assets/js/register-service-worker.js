if ("serviceWorker" in navigator) {
  window.addEventListener("load", function() {
    navigator.serviceWorker.register("/service-worker.js")
      .then(function(registration) {
        console.log("Registration succeeded. Scope is " + registration.scope);
      })
      .catch(function(error) {
        console.log("Registration failed with " + error);
      });
  });
}
