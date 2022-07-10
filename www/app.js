$(function() {

  $(document).on("keydown", function(event) {
    // Listen for Enter press
    var KEYCODE_ENTER = 13;
    if (event.keyCode == KEYCODE_ENTER && !$("#prevBtn").is(":focus") &&
        !document.getElementById('nextBtn').disabled) {
      $("#nextBtn").click();
    }

    // Listen to 1-9 key presses on likert scale inputs
    var KEYCODE_1 = 49;
    if (event.keyCode >= KEYCODE_1 && event.keyCode <= (KEYCODE_1 + 6)) {
      var num_pressed = event.keyCode - KEYCODE_1 + 1;
      var buttons = $("#main_area .page_likert_q").find("input[type='radio']:visible");
      if (buttons.length === 0) return;
      var button = buttons[num_pressed - 1];
      button.checked = true;
      $(button).closest(".shiny-input-container").trigger("change");
    }
  });
});
