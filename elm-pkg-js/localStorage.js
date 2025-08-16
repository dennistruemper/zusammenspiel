// LocalStorage functionality for Lamdera
// Following elm-pkg-js standards

exports.init = async function (app) {
  // Send initial active member ID to Elm
  const storedMemberId = localStorage.getItem("activeMemberId");
  if (storedMemberId) {
    app.ports.fromJS.send("LOAD:" + storedMemberId);
  } else {
    app.ports.fromJS.send("LOAD:null");
  }

  // Listen for commands from Elm
  app.ports.toJS.subscribe(function (message) {
    if (message.startsWith("SET:")) {
      const memberId = message.substring(4);
      localStorage.setItem("activeMemberId", memberId);
    } else if (message.startsWith("SET_ACCESS_CODE:")) {
      const parts = message.substring(16).split(":");
      const teamId = parts[0];
      const accessCode = parts[1];
      localStorage.setItem("teamAccessCode_" + teamId, accessCode);
    } else if (message.startsWith("GET_ACCESS_CODE:")) {
      const teamId = message.substring(16);
      const accessCode = localStorage.getItem("teamAccessCode_" + teamId);
      if (accessCode) {
        app.ports.fromJS.send(
          "ACCESS_CODE_LOADED:" + teamId + ":" + accessCode
        );
      } else {
        app.ports.fromJS.send("ACCESS_CODE_LOADED:" + teamId + ":");
      }
    } else if (message.startsWith("COPY:")) {
      const textToCopy = message.substring(5);
      navigator.clipboard
        .writeText(textToCopy)
        .then(() => {
          // Optional: Show a brief success message
          console.log("Copied to clipboard:", textToCopy);
        })
        .catch((err) => {
          console.error("Failed to copy to clipboard:", err);
        });
    } else if (message === "CLEAR") {
      localStorage.removeItem("activeMemberId");
    } else if (message === "GET") {
      const memberId = localStorage.getItem("activeMemberId");
      if (memberId) {
        app.ports.fromJS.send("LOAD:" + memberId);
      } else {
        app.ports.fromJS.send("LOAD:null");
      }
    } else if (message === "GET_HOSTNAME") {
      // Get current hostname with protocol
      const currentUrl = window.location.protocol + "//" + window.location.host;
      app.ports.fromJS.send("HOSTNAME:" + currentUrl);
    }
  });
};
