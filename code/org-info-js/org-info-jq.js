//
// org-info-jq.js
//
// Author and Copyright (c) 2009 Sebastian Rose, Hannover, Germany, sebastian_rose@gmx.de
//
// Contact:
//
//    Phone: +49 (0) 173 - 83 93 417
//    Email: sebastian_rose@gmx.de
//
// Released under the GNU General Public License version 3
// see: http://www.gnu.org/licenses/
//

var org_html_manager=
  {
    set: function(key, val){},
    setup: function(){},
    expanded: 0,
    contents: 1,
    folded: 2,
    VIEW: 'contents'
  };


$(document).ready(
  function(){
    for(var i=2;i<7;++i) {
      $("h"+i).each(
        function(){

          $(this).next("div").bind(
            "toggleForOrg",
            function(){
              if($(this).is(":visible"))
                $(this).trigger("hideForOrg");
              else
                $(this).trigger("showForOrg");
            });

          $(this).next("div").bind(
            "hideForOrg",
            function(){
              $(this).nextAll().hide();$(this).hide();
            });

          $(this).next("div").bind(
            "showForOrg",
            function(){
              $(this).parents().show();
              $(this).nextAll().show();$(this).show();
            });
          $(this).css({cursor: "pointer"});
          $(this).bind('click', function(){$(this).parent().children("div").eq(0).trigger("toggleForOrg");});
        });
    }
    if('contents' == org_html_manager.VIEW) {
      $("#text-table-of-contents").hide();
      for(var i=2;i<7;++i) {
        $(".outline-text-"+i).hide();
      }
    }
  });