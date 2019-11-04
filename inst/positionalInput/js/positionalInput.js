$(document).on("click", ".positional-input", function(evt) {

  // evt.currentTarget is the .context-menu-item that was clicked
  // evt.target returns the visible child element and is therefore not applicable
  var el = $(evt.currentTarget);

  // Set the elements data-value to its current data-value plus 1
  el.attr("data-value", parseInt(el.attr("data-value")) + 1);

  // Raise an event to signal that the value changed
  el.trigger("change");
});

var contextMenuItemBinding = new Shiny.InputBinding();
$.extend(contextMenuItemBinding, {
  find: function(scope) {
    return $(scope).find(".positional-input");
  },
  getValue: function(el) {
    var id = $(el).attr("id") + "_position";
    Shiny.setInputValue(id, el.getBoundingClientRect());
    return parseInt($(el).attr("data-value"));
  },
  subscribe: function(el, callback) {
    $(el).on("change.x", function(e) {
      callback();
    });
  },
  unsubscribe: function(el) {
    $(el).off(".x");
  }
});

Shiny.inputBindings.register(contextMenuItemBinding);