const nestJSON = function (original) {
  const nested = {};

  for (const [key, value] of Object.entries(original)) {
    const parts = key.split(".");
    let current = nested;

    for (let i = 0; i < parts.length - 1; i++) {
      const part = parts[i];
      if (!current[part]) {
        current[part] = {};
      }
      current = current[part];
    }

    current[parts[parts.length - 1]] = value;
  }

  return nested;
};

export const initMeasuringDuration = function (key) {
  return function () {
    try {
      window.events = window.events || {};
      if (typeof window.events[key] === "undefined")
        window.events[key] = new Date();
    } catch (error) {
      console.log("Latency start catch block" + error);
    }
  };
};

export const endMeasuringDuration = function (key) {
  return function () {
    try {
      window.events = window.events || {};
      if (typeof window.events[key] === "object")
        window.events[key] =
          new Date().getTime() - window.events[key].getTime();
    } catch (error) {
      console.log("Latency end catch block" + error);
    }
  };
};

export const getEvents = function () {
  try {
    if (typeof window === "object") {
      const events = Object.assign({}, window.events, {
        appVersion: window.version["app"],
        configVersion: window.version["configuration"],
        hyperCoreVersion: window.top.hyper_core_version,
        os: window.__OS,
        josBundleLoadTime:
          window.top.__osBundleLoadLogLine.os_bundle_load.bundle_load_latency,
        sdkVersion: window.__payload.sdkVersion,
        service: window.__payload.service,
        sessionId: window.session_id,
      });
      return JSON.stringify(nestJSON(events));
    } else {
      return JSON.stringify({});
    }
  } catch (error) {
    console.log("Latency getEvents catch block" + error);
    return JSON.stringify({});
  }
};
