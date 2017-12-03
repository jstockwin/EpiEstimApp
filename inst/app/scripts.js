$(document).ready(function() {
  document.getElementById("output").innerText = "Initialising...";

  Shiny.addCustomMessageHandler("error_box",
    function (id) {
      box = document.getElementById(id + "_error_box");
      box.style.border = '3px solid red';
    }
  );

  Shiny.addCustomMessageHandler("reset_error_boxes",
    function (unused) {
      boxes = document.getElementsByClassName("error_box");
      for (var i=0; i<boxes.length; i++) {
        boxes[i].style.border = 'none';
      }
    }
  );

  Shiny.addCustomMessageHandler("alert",function (msg) {alert(msg);});
});


