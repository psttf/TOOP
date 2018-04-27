var editor = ace.edit("code-editor");
var textarea = $('textarea[name="code"]').hide();
editor.getSession().setValue(textarea.val());
editor.getSession().on('change', function(){
  textarea.val(editor.getSession().getValue());
});