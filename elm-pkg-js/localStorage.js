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
    } else if (message === "GET_CURRENT_DATE") {
      // Get current date in German format (dd.mm.yyyy)
      const now = new Date();
      const day = String(now.getDate()).padStart(2, "0");
      const month = String(now.getMonth() + 1).padStart(2, "0"); // getMonth() returns 0-11
      const year = now.getFullYear();
      const germanDate = day + "." + month + "." + year;
      app.ports.fromJS.send("CURRENT_DATE:" + germanDate);
    } else if (message.startsWith("CONVERT_LOCAL_TO_UTC:")) {
      // Convert local date/time to UTC timestamp
      // Format: CONVERT_LOCAL_TO_UTC:22.09.2025|20:00
      const payload = message.substring(21);
      const parts = payload.split("|");
      const dateStr = parts[0];
      const timeStr = parts[1] || "00:00";
      const dateParts = dateStr.split(".");
      const timeParts = timeStr.split(":");

      if (dateParts.length === 3 && timeParts.length >= 2) {
        const day = parseInt(dateParts[0], 10);
        const month = parseInt(dateParts[1], 10) - 1;
        const year = parseInt(dateParts[2], 10);
        const hour = parseInt(timeParts[0], 10);
        const minute = parseInt(timeParts[1], 10);

        const localDate = new Date(year, month, day, hour, minute, 0);
        const utcYear = localDate.getUTCFullYear();
        const utcMonth = String(localDate.getUTCMonth() + 1).padStart(2, "0");
        const utcDay = String(localDate.getUTCDate()).padStart(2, "0");
        const utcHour = String(localDate.getUTCHours()).padStart(2, "0");
        const utcMinute = String(localDate.getUTCMinutes()).padStart(2, "0");

        const utcStr =
          String(utcYear) +
          utcMonth +
          utcDay +
          "T" +
          utcHour +
          utcMinute +
          "00Z";

        app.ports.fromJS.send("LOCAL_TO_UTC:" + utcStr);
      }
    } else if (message.startsWith("CONVERT_UTC_BATCH:")) {
      // Convert batch of UTC timestamps to local time
      // Format: CONVERT_UTC_BATCH:timestamp1,timestamp2,timestamp3,...
      const utcStrings = message.substring(18).split(",");
      const results = {};

      utcStrings.forEach((utcStr) => {
        if (utcStr && utcStr.length >= 15) {
          // Parse ISO format: 20250922T180000Z
          const year = parseInt(utcStr.substring(0, 4), 10);
          const month = parseInt(utcStr.substring(4, 6), 10) - 1; // JS months are 0-indexed
          const day = parseInt(utcStr.substring(6, 8), 10);
          const hour = parseInt(utcStr.substring(9, 11), 10);
          const minute = parseInt(utcStr.substring(11, 13), 10);
          const second = parseInt(utcStr.substring(13, 15), 10);

          // Create UTC date
          const utcDate = new Date(
            Date.UTC(year, month, day, hour, minute, second)
          );

          // Convert to local time strings
          const localDay = String(utcDate.getDate()).padStart(2, "0");
          const localMonth = String(utcDate.getMonth() + 1).padStart(2, "0");
          const localYear = utcDate.getFullYear();
          const localHour = String(utcDate.getHours()).padStart(2, "0");
          const localMinute = String(utcDate.getMinutes()).padStart(2, "0");

          const localDate = localDay + "." + localMonth + "." + localYear;
          const localTime = localHour + ":" + localMinute;

          results[utcStr] = localDate + "|" + localTime;
        }
      });

      // Send back as JSON-encoded map
      app.ports.fromJS.send("UTC_CONVERTED:" + JSON.stringify(results));
    }
  });
};
