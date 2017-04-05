$(document).ready(function() { 
  document.getElementById("output").innerText = "Initialising...";
  
  Shiny.addCustomMessageHandler("errorBox", 
    function (id) {
      box = document.getElementById(id + "ErrorBox");
      box.style = 'border-style: solid;';
    }
  );
  
  Shiny.addCustomMessageHandler("resetErrorBoxes", 
    function (unused) {
      boxes = document.getElementsByClassName("ErrorBox");
      for (var i=0; i<boxes.length; i++) {
        boxes[i].style = "border-style: none;";
      }
    }
  );
  
  Shiny.addCustomMessageHandler("alert",function (msg) {alert(msg);});
});

showMCMCParams = function() {
  document.getElementById('MCMCInitialParams').style.display='';
};
