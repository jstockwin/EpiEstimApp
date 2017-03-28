$(document).ready(function() { 
    go = function () {
      // The randomness in the following ensures that mydata really has changed. 
          // TODO: If mydata is the result of and MCMC fit and the data hasn't been changed, 
          // then we should try and keep mydata as-is, since then it'll not have to re-run mcmc.
          Shiny.onInputChange("status", "NEW" + Math.random());
    };
    setState = function (i) {
      i = Math.max(1, i);
      document.getElementById(1).hidden = true;
      document.getElementById(2).hidden = true;
      document.getElementById(3).hidden = true;
      document.getElementById(4).hidden = true;
      document.getElementById(5).hidden = true;
      document.getElementById(6).hidden = true;
      document.getElementById(7).hidden = true;
      document.getElementById(8).hidden = true;
      document.getElementById(9).hidden = true;
      document.getElementById(i).hidden = false;
      
      if ($('.final:visible').length > 0) {
        next.innerText = "Go";
      } else {
        next.innerText = "Next";
      }
    };
    var run = false;
    var next = document.getElementById("next");
    var prev = document.getElementById("prev");
    var output = document.getElementById("output");
    var continuedState;
    output.innerText = "Initialising...";
    setState(1);
    var progress = 1;
    next.onclick = function() {
      var final = $('.final:visible').length > 0;
      var cont = $('.continue:visible').length > 0;
      if ($('.progress-bar:visible').html() === "") {
        alert("You don't appear to have uploaded any data. Please try again before continuing");
        return null;
      }
      if (final) {
          run = true;
          next.disabled = true;
          next.innerText = "Running";
          prev.disabled = false;
          prev.innerText = "Stop";
          go();
          output.innerText = "Running...";
      } else if (cont) {
        // Cont. means we want to skip to state 5, as we're continuing
        // from the incidence data section to the SI section early.
        continuedState = progress;
        progress = 5;
        setState(progress);
        
      } else {
          progress += 1;
          setState(progress);
      }
    };
    prev.onclick = function() {
      if ($('.final:visible').length > 0 && run) {
        prev.innerText = "Stopping";
        prev.disabled = true;
        run = false;
        // The following does nothing, but will reset mydata, which is the trigger
        // for running the code in server.R.
        Shiny.onInputChange("status", "STOP");
      } else {
        if (progress == 5) {
          // We could have skipped to state 5 using continue.
          // Instead of just going down by 1, we'll go to the continuedState
          // which is the state we were in when a continue was triggered. 
          progress = continuedState;
        } else {
          progress += -1;
        }
        setState(progress);
      }
    };
    Shiny.addCustomMessageHandler("done", function(data) {
          run = false;
          next.disabled = false;
          prev.disabled = false;
          prev.innerText = "Previous";
          if ($('.final:visible').length > 0) {
            next.innerText = "Go";
          } else {
            next.innerText = "Next";
          }
          
          output.innerText = "Ready";
    });
    Shiny.addCustomMessageHandler("pingToClient",     
      function(data) {
        if (run) {
          if (data == "NEW") {
            // Server has said that our current mydata is outdated. Updating it to NEW
            // starts again.
            Shiny.onInputChange("mydata", "NEW");
            go();
          } else {
            // We have real data.
            // If we are still running, ping data straight back to client and hit go
            // but pause for a little first to keep the R thread free
            setTimeout(function () {Shiny.onInputChange("mydata", data); go();}, 10);
          }
        } else {
          // Else, stop.
          next.disabled = false;
          prev.disabled = false;
          prev.innerText = "Previous";
          next.innerText = "Go";
        }
      }
    );
    Shiny.addCustomMessageHandler("updateStatus", 
      function (message) {
        output.innerText = message;
      }
    );
    Shiny.addCustomMessageHandler("setMCMCInfo", function (obj) {
      // We only want to run MCMC if strictly necessary. To resolve this, when MCMC completes serverside
      // it will call this function. We then set various variables locally. Then, when we next make a call
      // to the server, this variables will be set. The server will do a comparison of these variables to
      // detemine if an MCMC re-run is necessary.
      obj = JSON.parse(obj);
      filepath = obj[0];
      dist = obj[1];
      param1 = obj[2];
      param2 = obj[3];
      Shiny.onInputChange("oldSIDatapath", filepath);
      Shiny.onInputChange("oldSIDist", dist);
      Shiny.onInputChange("oldParam1", param1);
      Shiny.onInputChange("oldParam2", param2);
      
    });
    
    Shiny.addCustomMessageHandler("popup", function(msg) {
      alert(msg);
    });
    
});

showMCMCParams = function() {
  document.getElementById('MCMCInitialParams').style.display='';
};
