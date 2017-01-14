$(document).ready(function() {   
    setState = function (i) {
      i = Math.min(6, Math.max(1, i));
      document.getElementById(1).hidden = true;
      document.getElementById(2).hidden = true;
      document.getElementById(3).hidden = true;
      document.getElementById(4).hidden = true;
      document.getElementById(5).hidden = true;
      document.getElementById(6).hidden = true;
      document.getElementById(i).hidden = false;
      
      if (i==6) {
        next.innerText = "Go";
      } else if (i == 5) {
        next.innerText = "Next";
      }
    };
    var run = false;
    var next = document.getElementById("next");
    var prev = document.getElementById("prev");
    var output = document.getElementById("output");
    output.innerText = "Initialising...";
    setState(1);
    var i = 0;
    var progress = 1;
    next.onclick = function() {

      if (progress == 6) {
          i = 0;
          run = true;
          next.disabled = true;
          next.innerText = "Running";
          prev.disabled = false;
          prev.innerText = "Stop";
          Shiny.onInputChange("mydata", "NEW");
          output.innerText = "Running... 0%";
      } else {
          progress += 1;
          setState(progress);
      }
    };
    prev.onclick = function() {
      if (progress == 6 && run) {
        prev.innerText = "Stopping";
        prev.disabled = true;
        run = false;
        Shiny.onInputChange("mydata", "STOP");
      } else {
        progress += -1;
        setState(progress);
      }
    };
    Shiny.addCustomMessageHandler("done", function(data) {
          run = false;
          next.disabled = false;
          prev.diabled = true;
          prev.innerText = "Prev";
          if (progress == 6) {
            next.innerText = "Go";
          } else {
            next.innerText = "Next";
          }
          
          output.innerText = "Ready";
    });
    Shiny.addCustomMessageHandler("pingToClient",     
      function(data) {
        if (run) {
          // If we are still running, ping data straight back to client
          // but pause for a little first to keep the R thread free
          i += 1;
          output.innerText = "Running... " + Math.round(i) + "%";
          setTimeout(Shiny.onInputChange("mydata", data), 10);
        } else {
          // Else, stop.
          next.disabled = false;
          prev.diabled = true;
          prev.innerText = "Prev";
          next.innerText = "Go";
        }
      }
    );
    
});