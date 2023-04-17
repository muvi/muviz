// #############################################################################
//   dp_sourcecode.js
//   v 1.0 - 08-FEB-2008
//   v 1.1 - 13-AUG-2008
//   v 1.2 - 18-FEB-2010
//   v 1.3 - 31-MRZ-2011 - jQuery added
//   v 1.4 - 24-JUN-2012 - minor updates
// #############################################################################
 
function dpX_selectcode( objID )
{
  var element = fetch_object(objID);
  
  if (document.selection && document.body.createTextRange && !is_opera)
  {
    var range = document.body.createTextRange();
    range.moveToElementText(element);
    range.select();
  }
  else
  if (window.getSelection && document.createRange)
  {
    var range = document.createRange();
    range.selectNodeContents( element );
    var selection = window.getSelection();
    selection.removeAllRanges();
    selection.addRange(range);
  }
  else
  {
    alert( 'Dein Browser unterstützt diese Funktion leider nicht.' );
  }
  return false;
}

var L_EXPANDTEXT2   = "aufklappen";
var L_COLLAPSETEXT2 = "zusammenfalten";
var clpHeight2 = "131px";

var code_heights2 = new Array()

function dpX_codefolding( objID, doAnimate )
{
  var code = '#code_' + objID;   
  var link = '#link_' + objID;
  
  if ($(code).css('height') != clpHeight2)
  {
    $(link).text( L_EXPANDTEXT2 );
    code_heights2[ code ] = $(code).height() + 'px';
     
    if (doAnimate)
    {
      $(code).css( {'overflow' : 'hidden', 'overflow-x' : 'hidden', 'overflow-y' : 'hidden'} );
      $(code).animate( { height: clpHeight2 }, 350, 'easeOutBack', function(){$(this).css( {'overflow' : 'auto', 'overflow-x' : 'auto', 'overflow-y' : 'auto'} ); } );
    }     
    else
      $(code).css({'height' : clpHeight2});
  }
  else
  {
    $(link).text( L_COLLAPSETEXT2 );
    
    if ((code_heights2[ code ] != null) && doAnimate)
    {
      $(code).css( {'overflow' : 'hidden', 'overflow-x' : 'hidden', 'overflow-y' : 'hidden'} );
      $(code).animate( { height: code_heights2[ code ] }, 350, 'easeOutBack', function(){$(this).css( {'overflow' : 'auto', 'overflow-x' : 'auto', 'overflow-y' : 'auto'} ); } );
    }  
    else       
      $(code).css({'height' : ''});
  }           
  return false; 
}