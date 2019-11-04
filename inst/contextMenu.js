Shiny.addCustomMessageHandler("show_contextmenu", function(message) {
  // Get exisiting wrapper DOM element, or create if needed.
  var $context_menu_wrapper = $('#context-menu-wrapper');
  if ($context_menu_wrapper.length === 0) {
   $context_menu_wrapper = $('<div id="context-menu-wrapper"><div>');
   $(document.body).append($context_menu_wrapper);
   $(document).on('click', function() {
     Shiny.unbindAll($context_menu_wrapper, true);
     /* setTimeout is used to delay the removal of the element. Otherwise the
     inputs inside of the context menu won't get updated upon clicking. */
     window.setTimeout(function() {
       $('.context-menu').remove();
     }, 1);
   });
  }
  
  Shiny.renderContent($context_menu_wrapper, message.content);
});