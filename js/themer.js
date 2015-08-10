function switch_style ( css_title )
{
// You may use this script on your site free of charge provided
// you do not remove this notice or the URL below. Script from
// http://www.thesitewizard.com/javascripts/change-style-sheets.shtml
  var i, link_tag ;
  for (i = 0, link_tag = document.getElementsByTagName("link") ;
    i < link_tag.length ; i++ ) {
    if ((link_tag[i].rel.indexOf( "stylesheet" ) != -1) &&
      link_tag[i].title) {
      link_tag[i].disabled = true ;
      if (link_tag[i].title == css_title) {
        link_tag[i].disabled = false ;
      }
    }
  }
}

var i, link_tag;
form = document.getElementById("buttons")
for (i=0, link_tag= document.getElementsByTagName("link");
        i < link_tag.length;i++){
    if ((link_tag[i].rel.indexOf("stylesheet") != -1) &&
            link_tag[i].title) {
        var button = document.createElement("input");
        button.setAttribute("type","submit");
        button.setAttribute("onclick","switch_style('"+link_tag[i].title+"');return false;");
        button.setAttribute("name","theme");
        button.setAttribute("value",link_tag[i].title);
        button.setAttribute("id",link_tag[i].title);
        form.appendChild(document.createTextNode(" "));
        form.appendChild(button);
    }
}
