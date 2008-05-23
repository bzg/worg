/**
 * @file
 *       org-info.js, v.0.0.6.0
 *
 * @author Sebastian Rose, Hannover, Germany - sebastian_rose at gmx dot de
 *
 *
 * This software is subject to the GNU General Public Licens version 2:
 * see: http://www.gnu.org/licenses/old-licenses/gpl-2.0.html
 *
 * Usage:
 *
 * Include this scipt into the Header of your HTML-exported org files by
 * customizing the variable org-export-html-style (TODO: add file local export
 * Options here).
 * You will also need this here somewhere in the HTML-page:
 *
 * <script type="text/javascript" language="JavaScript" src="org-info.js"></script>
 * <script type="text/javascript" language="JavaScript">
 *  <![CDATA[ // comment around this one
 *  org_html_manager.set("LOCAL_TOC", "1");
 *  org_html_manager.set("VIEW", "info");
 *  org_html_manager.set("VIEW_BUTTONS", "true");
 *  org_html_manager.set("LINK_UP", "http://full/path/to/index/of/this/directory.html");
 *  org_html_manager.set("LINK_HOME", "http://full/path/to/homepage.html");
 *  org_html_manager.set("MOUSE_HINT", "underline"); // or a color like '#eeeeee'
 *  org_html_manager.setup ();
 *  ]]> // comment around this one
 * </script>
 *
 * @todo
 * You may use the new export options template line #+INFOJS_OPT: view:info ...
 */


/**
 * Creates a new OrgNode.
 * An OrgOutline stores some refs to its assoziated node in the document tree
 * along with some additional properties.
 */
function OrgNode ( _div, _heading, _link, _depth, _parent, _base_id)
{
  this.div = _div;
  this.base_id = _base_id;
  this.idx = -1;                          // The index in OrgHtmlManager::SECS[]
  this.heading = _heading;
  this.link = _link;
  this.parent = _parent;
  this.durty = false;                     // Node is durty, when children get
                                          // folded seperatly.
  this.state = OrgNode.STATE_FOLDED;
  this.depth = _depth;                    // The Rootelement will have
                                          // the depth 0. All other
                                          // Nodes get the depth from
                                          // their h-level (h1, h2...)
  this.folder = null;
  this.children = new Array();
  this.info_navigation = "";
  this.buttons = null;

  if(null != this.parent) {
    this.parent.addChild(this);
    this.hide();
  }

  var folder = document.getElementById("text-"+this.base_id);
  if(null != folder) {
    this.folder = folder;
  }

}

// static variables
OrgNode.STATE_FOLDED = 0;
OrgNode.STATE_HEADLINES = 1;
OrgNode.STATE_UNFOLDED = 2;

//
// static functions
//

OrgNode.hideElement = function (e)
{
  if(e) {
    e.style.display = 'none';
    e.style.visibility = 'hidden';
  }
};

OrgNode.showElement = function (e)
{
  if(e) {
    e.style.display = 'block';
    e.style.visibility = 'visible';
  }
};

//
// member functions
//

OrgNode.prototype.addChild = function (child)
{
  this.children.push(child);
  return this.parent;
};

OrgNode.prototype.getParent = function ()
{
  return this.parent;
};

//
// OrgNode methods for paging (info mode)
//

OrgNode.prototype.hide = function ()
{
    OrgNode.hideElement(this.div);
    if(this.parent)
      this.parent.hide();
};

OrgNode.prototype.show = function ()
{
  OrgNode.showElement(this.div);
  if(this.depth > 2)
    this.parent.show();
};

OrgNode.prototype.showAllChildren = function ()
{
  for(var i=0;i<this.children.length;++i) { this.children[i].showAllChildren(); }
  this.show();
};

OrgNode.prototype.hideAllChildren = function ()
{
  for(var i=0;i<this.children.length;++i) { this.children[i].hideAllChildren(); }
  this.hide();
};


//
//  OrgNode methods for folding
//

/**
 * This one is called onclick() to toggle the folding state of the node.
 * This one sets it's parent durty, since node is folded individually. Hence the
 * next folding of parent has to collapse all.
 * @param show_childrens_folders Boolean. This is only used for the special way
 * of toggling of the ROOT element. If true, prevents this OrgNode from showing
 * the folder.
 */
OrgNode.prototype.fold = function (hide_folder)
{
  if(this.parent)
    this.parent.durty = true;
  if(this.durty) {
    this.durty = false;
    this.state = OrgNode.STATE_UNFOLDED; // so next state is FOLDED. See below.
  }

  if(null != this.folder) {

    if(this.state == OrgNode.STATE_FOLDED) {
      // I was folded but one could click on me. So now show Headlines
      // recursive.
      if(this.children.length) {
        this.state = OrgNode.STATE_HEADLINES;
        OrgNode.hideElement(this.folder);
        for(var i=0;i<this.children.length;++i) { this.children[i].setState(OrgNode.STATE_HEADLINES); }
      } else if (! hide_folder) {
        // without children jump to unfolded state:
        this.state = OrgNode.STATE_UNFOLDED;
        OrgNode.showElement(this.folder);
      }
    }
    else if(this.state == OrgNode.STATE_HEADLINES) {
      // show all content recursive
      this.state = OrgNode.STATE_UNFOLDED;
      OrgNode.showElement(this.folder);
      for(var i=0;i<this.children.length;++i) { this.children[i].setState(OrgNode.STATE_UNFOLDED); }
    }
    else {
      // collapse. Show only own headline
      this.state = OrgNode.STATE_FOLDED;
      OrgNode.hideElement(this.folder);
      for(var i=0;i<this.children.length;++i) { this.children[i].setState(OrgNode.STATE_FOLDED); }
    }
  }
  else
    alert("folder == null\nCheck your org export version.");
};

/**
 * Recursive state switching. This functions folds children of activated
 * parents. The states have a slightly different meaning here. Here the
 * surrounding div (outline-container-id) gets hidden too.
 * Maybe add OrgNode.STATE_HIDDEN with same value?
 */
OrgNode.prototype.setState = function (state)
{
  for(var i=0;i<this.children.length;++i) {
    this.children[i].setState(state);
  }
  switch (state)
    {
      case OrgNode.STATE_FOLDED:
        OrgNode.hideElement(this.folder);
        OrgNode.hideElement(this.div);
      break;
      case OrgNode.STATE_HEADLINES:
        OrgNode.hideElement(this.folder);
        OrgNode.showElement(this.div);
      break;
      default:
        OrgNode.showElement(this.folder);
        OrgNode.showElement(this.div);
    }
  this.state = state;
};



/**
 * OrgManager manages OrgNodes.
 * We don't create anything in the constructor, since the document is not loaded
 * yet.
 */
var org_html_manager = {
  // Option
  MOUSE_HINT: 0,               // Highlight heading under mouse?
  PLAIN_VIEW: 0,               // We're in plain view mode. On startup:= overview
  CONTENT_VIEW: 1,             // plain view show structure
  ALL_VIEW: 2,                 // plain view show all
  INFO_VIEW : 3,               // We're in info view mode
  VIEW: this.OVER_VIEW,        // Default view mode (s. setup())
  LOCAL_TOC: false,            // Create sub indexes (s. setup())
  LINK_HOME: 0,                // Link to this.LINK_HOME?
  LINK_UP: 0,                  // Link to this.LINK_UP?
  LINKS: "",                   // Prepare the links for later use (see setup),
  RUN_MAX: 1200,               // Max attempts to scan (results in ~2 minutes)
  RUN_INTERVAL: 100,           // Interval of scans in milliseconds.
  DEBUG: 0,                    // Gather and show debugging info?
  WINDOW_BORDER: false,        // Draw a border aroung info window
  HIDE_TOC: false,             // Hide the table of contents.
  TOC_DEPTH: 0,                // Level to cut the table of contents. No cutting if 0.

  // Private
  BASE_URL: document.URL,      // URL without '#sec-x.y.z'
  START_SECTION: 0,            // Will be evtl. recomputed from '#sec-x.y.z'
  ROOT: null,                  // Root element or our OrgNode tree
  NODE: null,                  // The current node
  TITLE: null,                 // Save the title for hide/show
  LOAD_CHECK: null,            // Saves the setTimeout()'s value
  WINDOW: null,                // A div to display info view mode
  SECS: new Array(),           // The OrgNode tree
  REGEX: /(#sec\-)([\d\.]*$)/, // identify a section link in toc
  EXCHANGE: /(sec-[\d\.]*)$/,  // extract the section number
  UNTAG_REGEX: /<[^>]+>/i,     // Remove HTML tags
  EMPTY_START: /^(\s*)(.*)/,   // Trim (s. getKey())
  EMPTY_END: /\s$/,            // Trim (s. getKey())
  SECEX: /([\d\.]*)/,          // Section number (command 's')
  TOC: null,                   // toc.
  runs: 0,                     // Count the scan runs.
  HISTORY: new Array(50),      // Save navigation history.
  HIST_INDEX: 0,
  SKIP_HISTORY: false,         // popHistory() set's this to true.
  FIXED_TOC: false,            // Leave toc alone if position=[fixed|absolute]?
  // Debugging:
  debug: "",                   // Will be shown after every scan, if not empty
  DEBUG_FATAL: 1,              // Fatale Fehler anzeigen.
  DEBUG_BUILD: 1 << 5,
  DEBUG_TREE: 1 << 10,
  WINDOW_BORDER: false,        // Draw a border aroung info window
  // Commands:
  CONSOLE: null,               // The div containing the minibuffer.
  CONSOLE_INPUT: null,
  command_str: "",
  console_first_time: true,    // Cookie would be cool maybe.
  MESSAGING:0,                 // Is there a message in the console?
  MESSAGING_INPLACE: 1,
  MESSAGING_TOP: 2,
  // show help:
  helping:false,
  last_view_mode:0,
  TAB_INDEX: 1000,             // Users could have defined tabindexes!


  /**
   * Setup the OrgHtmlManager for scanning.
   * Here the timeout function gets set, that tells the wakes up org_html_mager
   * for the next attempt to scan the document.
   * All user setable config vars (the documented ones) are checked and adjusted
   * to a legal value.
   */
  setup: function ()
  {
    if(location.search) { // enable overwriting of settings
      var sets = location.search.substring(1).split('&');
      for(var i = 0; i < sets.length; ++i) {
        var pos = sets[i].indexOf('=');
        if(-1 != pos) {
          var v = sets[i].substring(pos+1);
          var k = sets[i].substring(0, pos);
          switch(k) {
            // Explicitely allow overwrites.
            // Fall through:
          case 'TOC':
          case 'TOC_DEPTH':
          case 'MOUSE_HINT':
          case 'VIEW':
          case 'HIDE_TOC':
          case 'LOCAL_TOC':
          case 'VIEW':
            this.set(k, decodeURIComponent(v));
            break;
          default: break;
          }
        }
      }
    }
    this.VIEW  = this.VIEW ? this.VIEW : this.PLAIN_VIEW;
    this.VIEW_BUTTONS = (this.VIEW_BUTTONS && this.VIEW_BUTTONS != "0") ? true : false;
    this.LOCAL_TOC = (this.LOCAL_TOC && this.LOCAL_TOC != "0") ? true : false;
    this.HIDE_TOC = (this.TOC && this.TOC != "0") ? false : true;
    if(this.FIXED_TOC && this.FIXED_TOC != "0") {
      this.FIXED_TOC = 1;
      this.HIDE_TOC = false;
    }

    this.LINKS +=
    ((this.LINK_UP && this.LINK_UP != document.URL) ? '<a href="'+this.LINK_UP+'">Up</a> / ' : "")
      + ((this.LINK_HOME && this.LINK_HOME != document.URL) ? '<a href="'+this.LINK_HOME+'">HOME</a> / ' : "");

    this.LOAD_CHECK = window.setTimeout("OrgHtmlManagerLoadCheck()", 50);
  },


  removeTags: function (str)
  {
    while(str.match(this.UNTAG_REGEX)) {
      str = str.substr(0, str.indexOf('<')) + str.substr(str.indexOf('>') + 1);
      if(this.DEBUG > 5) this.debug += str + "\n";
    }
    return str;
  },


  init: function ()
  {
    this.runs++;
    if(1 ==  this.runs) {
      this.WINDOW = document.createElement("div");
      if(this.WINDOW_BORDER) this.WINDOW.style.border="1px dashed black";
    }
    var theIndex = document.getElementById('table-of-contents');
    var scanned_all = false;
    if(null != theIndex) {
      if(this.initFromTOC()) {
        scanned_all = true;
      }
    }

    if(this.BASE_URL.match(this.REGEX)) {
      // cut the '#sec-x.y.z' from base url.
      var matches = this.EXCHANGE.exec(this.BASE_URL);
      var id = matches[1].substr(4);
      for(var i=0;i<this.SECS.length;++i) {
        if(this.SECS[i].base_id == id) {
          this.START_SECTION = i;
          break;
        }
      }
      if(location.search)
        this.BASE_URL = this.BASE_URL.substring(0, this.BASE_URL.indexOf('?'));
      else
        this.BASE_URL = this.BASE_URL.substring(0, this.BASE_URL.indexOf('#'));
    }

    this.convertLinks(); // adjust internal links. BASE_URL has to be stripped.

    if(scanned_all) {
      if(this.VIEW == this.INFO_VIEW) {
        this.infoView(this.START_SECTION);
      }
      else {
        var v = this.VIEW; // will be changed in this.plainView()!
        this.plainView(this.START_SECTION);
        this.ROOT.dirty = true;
        this.ROOT_STATE = OrgNode.STATE_UNFOLDED;
        this.toggleGlobaly();
        if(v > this.PLAIN_VIEW) {
          this.toggleGlobaly();
        }
        if (v == this.ALL_VIEW) {
          this.toggleGlobaly();
        }
      }
      if(this.START_SECTION) this.showSection(this.START_SECTION);
      else window.scrollTo(0, 0);
    }
    else {
      if( this.runs < this.RUN_MAX ) {
        this.LOAD_CHECK = window.setTimeout("OrgHtmlManagerLoadCheck()", this.RUN_INTERVAL);
        return;
      }
      // TODO: warn if not scanned_all
    }

    this.CONSOLE = document.createElement("div");
    this.CONSOLE.innerHTML = '<form action="" onsubmit="return false;">'
      +'<input type="text" id="org-console-input" onkeydown="org_html_manager.getKey();this.blur();"'
      +' onclick="this.select();" maxlength="150" style="width:100%;border:1px inset #dddddd;"'
      +' value=""/>'
      +'</form>';
    this.CONSOLE.style.position = 'fixed';
    this.CONSOLE.style.marginTop = '-40px';
    this.CONSOLE.style.top = '-40px';
    this.CONSOLE.style.left = '0px';
    this.CONSOLE.style.width = '100%';
    this.CONSOLE.style.height = '30px';

    document.body.insertBefore(this.CONSOLE, document.body.firstChild);
    this.MESSAGING = false;
    this.CONSOLE_INPUT = document.getElementById("org-console-input");
    this.CONSOLE_INPUT.style.marginTop = '-40px';
    document.onkeypress=OrgHtmlManagerKeyEvent;

    if(0 != this.DEBUG && this.debug.length) alert(this.debug);
  },


  initFromTOC: function ()
  {
    // scan the document for sections. We do it by scanning the toc,
    // so we do what is customized for orgmode (depth of sections in toc).
    if(this.runs == 1 || ! this.ROOT) {
      var toc = document.getElementById("table-of-contents");
      if(null != toc) {
        var heading = null;
        var i = 0;
        for(i;heading == null && i < 7;++i) // TODO: What is this?
          heading = toc.getElementsByTagName("h"+i)[0];
        heading.onclick = function() {org_html_manager.fold(0);};
        //heading.setAttribute('onclick', 'org_html_manager.fold(0);');
        heading.style.cursor = "pointer";
        if(this.MOUSE_HINT) {
          heading.onmouseover = function(){org_html_manager.highlight_headline(0);};
          heading.onmouseout = function(){org_html_manager.unhighlight_headline(0);};
          // heading.setAttribute('onmouseover', 'org_html_manager.highlight_headline(this);');
          // heading.setAttribute('onmouseout', 'org_html_manager.unhighlight_headline(this);');
        }

        if(this.FIXED_TOC) {
          heading.setAttribute('onclick', 'org_html_manager.toggleGlobaly();');
          this.ROOT = new OrgNode( null,
                                   document.body.getElementsByTagName("h1")[0],
                                   'javascript:org_html_manager.navigateTo(0);',
                                   0,
                                   null ); // the root node
          this.TOC = new OrgNode( toc,
                                  heading,
                                  'javascript:org_html_manager.navigateTo(0);',
                                  i,
                                  null ); // the root node
          this.NODE = this.ROOT;
        }
        else {
          this.ROOT = new OrgNode( null,
                                   document.body.getElementsByTagName("h1")[0],
                                   'javascript:org_html_manager.navigateTo(0);',
                                   0,
                                   null ); // the root node
          if(this.HIDE_TOC) {
            this.TOC = new OrgNode( toc,
                                    "",
                                    'javascript:org_html_manager.navigateTo(0);',
                                    i,
                                    null );
            this.NODE = this.ROOT;
            OrgNode.hideElement(toc);
          }
          else {
            this.TOC = new OrgNode( toc,
                                    heading,
                                    'javascript:org_html_manager.navigateTo(0);',
                                    i,
                                    this.ROOT ); // the root node
            this.TOC.idx = 0;
            this.NODE = this.TOC;
            this.SECS.push(this.TOC);
          }
        }
        if(this.TOC) this.TOC.folder = document.getElementById("text-table-of-contents");
      }
      else
        return false;
    }

    var theIndex = document.getElementsByTagName("ul")[0]; // toc

    // Could we scan the document all the way down?
    // Return false if not.
    if(! this.ulToOutlines(theIndex))
      return false;

    if(this.TOC_DEPTH) {
      this.cutToc(theIndex, 1);
    }

    // Move the title into the first visible section.
    // TODO: show title above everything if FIXED_TOC !!!
    this.TITLE = document.getElementsByTagName("h1")[0];
    if(!this.FIXED_TOC) {
      var title = this.TITLE.cloneNode(true);
      this.SECS[0].div.insertBefore(title, this.SECS[0].div.firstChild);
      OrgNode.hideElement(this.TITLE);
    }

    // Create all the navigation links:
    this.build();
    this.NODE = this.SECS[0];

    document.body.insertBefore(this.WINDOW, this.NODE.div);

    return true;
  },

  /**
   * Used by OrgHtmlManager::initFromToc
   */
  ulToOutlines: function (ul)
  {
    if(ul.hasChildNodes() && ! ul.scanned_for_org) {
      for(var i=0; i<ul.childNodes.length; ++i) {
        if(false == this.liToOutlines(ul.childNodes[i])) {
          //this.debug += "ulToOutlines: stopped. "+this.SECS.length + " Childnodes scanned.";
          return false;
        }
      }
      ul.scanned_for_org = 1;
    }
    return true;
  },

  /**
   * Used by OrgHtmlManager::initFromToc
   */
  liToOutlines: function (li)
  {
    if(! li.scanned_for_org) {
      for(var i=0; i<li.childNodes.length; ++i) {
        var c = li.childNodes[i];
        switch (c.nodeName) {
        case "A":
          //this.debug += c.href + "\n";
          var newHref = this.mkNodeFromHref(c.href);
          if(false == newHref) {
            this.debug += "liToOutlines: stopped\n";
            return false;
          }
          else {
            c.href = newHref;
            c.tabIndex = this.TAB_INDEX;
            this.TAB_INDEX++;
          }
          break;
        case "UL":
          return this.ulToOutlines(c);
          break;
        }
      }
      li.scanned_for_org = 1;
    }
  },

  /**
   * Used by OrgHtmlManager::initFromToc
   */
  cutToc: function (ul, cur_depth)
  {
    cur_depth++;
    if(ul.hasChildNodes()) {
      for(var i=0; i < ul.childNodes.length; ++i) {
        var li = ul.childNodes[i];
        for(var j=0; j < li.childNodes.length; ++j) {
          var c = li.childNodes[j];
          if(c.nodeName == "UL") {
            if(cur_depth > this.TOC_DEPTH)
              li.removeChild(c);
            else
              this.cutToc(c, cur_depth); }}}}
  },

  /**
   * Used by OrgHtmlManager::liToOutlines
   */
  mkNodeFromHref: function (s)
  {
    if(s.match(this.REGEX)) {
      var matches = this.EXCHANGE.exec(s);
      var ret = 'javascript:org_html_manager.navigateTo("'+matches[1]+'");';
      this.debug += matches[0]+"\n";
      this.debug += " -> linked section: " + matches[1].substr(4) + "\n";
      var heading = document.getElementById(matches[1]);
      // This heading could be null, if the document is not yet entirely loaded.
      // So we stop scanning and set the timeout function in caller.
      // We could even count the <ul> and <li> elements above to speed up the next
      // scan.
      if(null == heading) {
        this.debug += ("heading is null. Scanning stopped.\n");
        return(false);
      }
      var div = heading.parentNode; // seems to work great in browsers
      var sec = this.SECS.length;
      var depth = div.className.substr(8);
      var id = matches[1].substr(4);
      heading.onclick = function() {org_html_manager.fold("" + sec);};
      // heading.setAttribute('onclick', 'org_html_manager.fold(' + sec + ');');
      heading.style.cursor = "pointer";
      if(this.MOUSE_HINT) {
        heading.onmouseover = function() {org_html_manager.highlight_headline("" + sec);};
        heading.onmouseout = function() {org_html_manager.unhighlight_headline("" + sec);};
        // heading.setAttribute('onmouseover', 'org_html_manager.highlight_headline(this);');
        // heading.setAttribute('onmouseout', 'org_html_manager.unhighlight_headline(this);');
      }
      var link = 'javascript:org_html_manager.navigateTo(' + sec + ')';
      // Is this wrong (??):
      if(depth > this.NODE.depth) {
        this.NODE = new OrgNode ( div, heading, link, depth, this.NODE, id);
      }
      else if (depth == 2) {
        this.NODE = new OrgNode ( div, heading, link, depth, this.ROOT, id);
      }
      else {
        var p = this.NODE;
        while(p.depth > depth) p = p.parent;
        this.NODE = new OrgNode ( div, heading, link, depth, p.parent, id);
      }
      this.SECS.push(this.NODE);
      this.NODE.hide();
      return (link);
    }
    // return the link as it was, if no section link:
    return (s);
  },

  /**
   * Creates all the navigation links for this.SECS.
   * This is called from initFromStructure() and initFromToc() as well.
   *
   * @todo Apply style classes to the generated links.
   */
  build: function ()
  {
    var index_name =  this.TITLE.innerHTML;
    var min_subindex_sec = 0;

    for(var i = 0; i < this.SECS.length; ++i)
    {
      this.SECS[i].idx = i;
      var html = '<table class="org-info-info-navigation" width="100%" border="0" style="border-bottom:1px solid black;">'
        +'<tr><td colspan="3" style="text-align:left;border-style:none;vertical-align:bottom;">'
        +'<span style="float:left;display:inline;text-align:left;">'
        +'Top: <a accesskey="i" href="javascript:org_html_manager.navigateTo(0);">'+index_name+'</a></span>'
        +'<span style="float:right;display:inline;text-align:right;font-size:70%;">'
        + this.LINKS
        +'<a accesskey="t" href="javascript:org_html_manager.toggleView('+i+');">toggle view</a></span>'
        +'</td></tr><tr><td style="text-align:left;border-style:none;vertical-align:bottom;width:22%">';

      if(i>1) // was > 0
        html += '<a accesskey="p" href="'+this.SECS[i-1].link
        +'" title="Go to: '+this.removeTags(this.SECS[i-1].heading.innerHTML)+'">Previous</a> | ';
      else
        html += 'Previous | ';

      if(i < this.SECS.length - 1)
        html += '<a accesskey="n" href="'+this.SECS[i+1].link
        +'" title="Go to: '+this.removeTags(this.SECS[i+1].heading.innerHTML)+'">Next</a>';
      else
        html += 'Next';

      html += '</td><td style="text-align:center;vertical-align:bottom;border-style:none;width:56%;">';

      if(i>0 && this.SECS[i].parent.parent) // != this.ROOT)
        html += '<a href="'+this.SECS[i].parent.link
        +'" title="Go to: '+this.removeTags(this.SECS[i].parent.heading.innerHTML)+'">'
        +'<span style="font-variant:small-caps;font-style:italic;">'
        +this.SECS[i].parent.heading.innerHTML+'</span></a>';
      else
        html += '<span style="font-variant:small-caps;font-style:italic;">'+this.SECS[i].heading.innerHTML+'</span>';

      // Right:
      html += '</td><td style="text-align:right;vertical-align:bottom;border-style:none;width:22%">';
      html += (i + 1) +'</td></tr></table>';

      // buttons:
      this.SECS[i].buttons = document.createElement("div");
      this.SECS[i].buttons.innerHTML = '<div style="display:inline;float:right;text-align:right;font-size:70%;font-weight:normal;">'
        + this.LINKS
        + '<a accesskey="t" href="javascript:org_html_manager.toggleView('+i+');">toggle view</a></div>';
      if(this.SECS[i].folder)
        // this.SECS[i].heading.appendChild(this.SECS[i].buttons);
        this.SECS[i].div.insertBefore(this.SECS[i].buttons, this.SECS[i].heading); //div.firstChild.nextSibling);
      else if(this.SECS[i].div.hasChildNodes()) {
        this.SECS[i].div.insertBefore(this.SECS[i].buttons, this.SECS[i].div.firstChild);
      }
      if(!this.VIEW_BUTTONS) OrgNode.hideElement(this.SECS[i].buttons);
      this.SECS[i].navigation = html;

      // subindex for sections containing subsections:
      if(0 < this.SECS[i].children.length && this.LOCAL_TOC)
      {
        var navi2 = document.createElement("div");
        html = 'Contents:<br /><ul>'; // <li>'+this.SECS[i].heading.innerHTML+'<ul>';
        for(var k=0; k < this.SECS[i].children.length; ++k) {
          html += '<li><a href="'
            +this.SECS[i].children[k].link+'">'
            +this.SECS[i].children[k].heading.innerHTML+'</a></li>';
        }
        html += '</ul>'; // </li></ul>';
        navi2.innerHTML = html;
        if(this.SECS[i].folder)
          this.SECS[i].folder.insertBefore(navi2, this.SECS[i].folder.firstChild);
        else
          this.SECS[i].div.insertBefore
        (navi2, this.SECS[i].div.getElementsByTagName("h"+this.SECS[i].depth)[0].nextSibling);
      }
    }
  },

  showSection: function (sec)
  {
    var section = parseInt(sec);
    var last_node = this.NODE;
    if(this.HIDE_TOC && this.NODE == this.TOC && !this.FIXED_TOC) {
      OrgNode.hideElement(this.TOC.div);
      if(this.PLAIN_VIEW == this.VIEW) {
        this.ROOT.showAllChildren();
        for(var i=0;i<this.ROOT.children.length;++i) {
          this.ROOT.children[i].state = OrgNode.STATE_UNFOLDED;
          this.ROOT.children[i].fold();
        }
      }
    }
    if('toc' == sec || (!isNaN(section) && this.SECS[section])) {
      if('toc' == sec && this.HIDE_TOC)
        {
          this.NODE = this.TOC;
          this.ROOT.hideAllChildren();
          if(this.INFO_VIEW == this.VIEW)
            this.WINDOW.innerHTML =  this.NODE.div.innerHTML;
          else
            this.NODE.setState(OrgNode.STATE_UNFOLDED);
          window.scrollTo(0, 0);
        }
      else
        {
          this.NODE = this.SECS[section];
          if(this.INFO_VIEW == this.VIEW) {
            if(this.VIEW_BUTTONS) OrgNode.showElement(last_node.buttons);
            OrgNode.hideElement(this.NODE.buttons);
            this.NODE.setState(OrgNode.STATE_UNFOLDED);
            for(var i=0;i<this.NODE.children.length; ++i)
              this.NODE.children[i].hide();
            this.WINDOW.innerHTML =  this.NODE.navigation + this.NODE.div.innerHTML;
            this.NODE.hide();
            window.scrollTo(0, 0);
          }
          else {
            if(! this.VIEW_BUTTONS) OrgNode.hideElement(last_node.buttons);
            OrgNode.showElement(this.NODE.buttons);
            this.NODE.setState(OrgNode.UNFOLDED);
            this.NODE.show();
            if(0 < this.NODE.idx)
              this.NODE.div.scrollIntoView(true);
            else
              window.scrollTo(0, 0);
          }
        }
    }
  },


  plainView: function (sec)
  {
    this.VIEW = this.PLAIN_VIEW;
    OrgNode.hideElement(this.WINDOW);
    // OrgNode.showElement(this.TITLE);
    // For Opera and accesskeys we have to remove the navigation here to get it
    // working when toggeling back to info view again:
    if(this.WINDOW.firstChild) // might not be set after init
      this.WINDOW.removeChild(this.WINDOW.firstChild);
    this.ROOT.showAllChildren();
    for(var i=0;i<this.ROOT.children.length;++i) {
      this.ROOT.children[i].state = OrgNode.STATE_UNFOLDED;
      this.ROOT.children[i].fold();
    }
    this.showSection(sec);
    this.NODE.div.scrollIntoView(true);
  },

  infoView: function (sec, skip_show_section)
  {
    this.VIEW = this.INFO_VIEW;
    this.unhighlight_headline(this.NODE.idx);//heading);
    if(!this.FIXED_TOC)
      OrgNode.hideElement(this.TITLE);
    OrgNode.showElement(this.WINDOW);
    this.ROOT.hideAllChildren();
    if(this.TOC && !this.FIXED_TOC) OrgNode.hideElement(this.TOC.div);
    if(!skip_show_section)
      this.showSection(sec);
  },

  toggleView: function (sec)
  {
    this.removeWarning();
    if(this.VIEW == this.INFO_VIEW)
      this.plainView(sec);
    else
      this.infoView(sec);
  },

  fold: function (sec)
  {
    this.removeWarning();
    var section = parseInt(sec);
    this.SECS[section].fold();
    if(! this.VIEW_BUTTONS) OrgNode.hideElement(this.NODE.buttons);
    this.NODE = this.SECS[section];
    OrgNode.showElement(this.NODE.buttons);
    if(this.INPUT_FIELD) this.INPUT_FIELD.focus();
  },

  highlight_headline: function(h)
  {
    var i = parseInt(h);
    if(this.PLAIN_VIEW == this.VIEW && this.MOUSE_HINT) {
      if('underline' == this.MOUSE_HINT)
        this.SECS[i].heading.style.borderBottom = "1px dashed #666666";
      // h.style.borderBottom = "1px dashed #666666";
      else
        this.SECS[i].heading.style.backgroundColor = this.MOUSE_HINT;
      //h.style.backgroundColor = this.MOUSE_HINT;
    }
  },


  unhighlight_headline: function(h)
  {
    var i = parseInt(h);
    if('underline' == this.MOUSE_HINT) {
      this.SECS[i].heading.style.borderBottom = "";
    }
    else
      this.SECS[i].heading.style.backgroundColor = "";
  },

  /**
   * Execute arbitrary JavaScript code. This one is used for configuration.
   */
  set: function (eval_key, eval_val)
  {
    if("VIEW" == eval_key) {
      var overview = this.PLAIN_VIEW;
      var content = this.CONTENT_VIEW;
      var showall = this.ALL_VIEW;
      var info = this.INFO_VIEW;
      eval("this."+eval_key+"="+eval_val+";");
      return;
    }

    if(eval_val)
      eval("this."+eval_key+"='"+eval_val+"';");
    else
      eval("this."+eval_key+"=0;");
  },


  /**
   * All commands that add something to the history should return.
   */
  getKey: function ()
  {
    var s = this.CONSOLE_INPUT.value;
    // return, if s is empty:
    if(0 == s.length) return;

    var copy = false;

    // the easiest is to just drop everything and clean the console.
    // User has to retype again.
    if(this.MESSAGING) {
      this.removeWarning();
    }
    else if(this.helping) {
      this.showHelp();
      this.command_str = "";
      this.CONSOLE_INPUT.value = "";
      return;
    }

    // Always remove TOC from history, if HIDE_TOC
    if(this.HIDE_TOC && this.TOC == this.NODE && "v" != s && "V" != s) {
      s = "b";
    }
    else {
      if(s.match(this.EMPTY_START))
        s = s.match(this.EMPTY_START)[2];
      if(s.length && s.match(this.EMPTY_END))
        s = s.substr(0, s.length - 1);
    }

    if (1 == s.length)    // one char wide commands
      {
        if ('n' == s) {
          if(this.NODE.state == OrgNode.STATE_FOLDED && this.VIEW == this.PLAIN_VIEW) {
            this.showSection(this.NODE.idx);
          }
          else if(this.NODE.idx < this.SECS.length - 1) {
            this.navigateTo(this.NODE.idx + 1);
            return;
          } else {
            this.warn("Already last section.");
            return;                          // rely on what happends if messaging
          }
        }
        else if ('p' == s) {
          if(this.NODE.idx > 0) {
            this.navigateTo(this.NODE.idx - 1);
            return;
          } else {
            this.warn("Already first section.");
            return;                          // rely on what happends if messaging
          }
        }
        else if ('b' == s) {
          this.popHistory();
          return;
        }
        else if ('B' == s) {
          this.popHistory(true);
          return;
        }
        else if ('q' == s) {
          if(window.confirm("Really close this file?")) {
            window.close();
          }
        }
        else if ('i' == s) {
          if(this.FIXED_TOC) {
            this.TOC.folder.getElementsByTagName("A")[0].focus();
          }
          else if (this.HIDE_TOC) this.navigateTo('toc');
          else if(0 != this.NODE.idx) this.navigateTo(0);
        }
        else if ('m' == s) {
          this.toggleView(this.NODE.idx);
        }
        else if ('<' == s || 't' == s) {
          if(0 != this.NODE.idx) this.navigateTo(0);
          else window.scrollTo(0,0);
        }
        else if ('>' == s || 'E' == s) { // 'e' scrolls down in opera :-/
          if((this.SECS.length - 1) != this.NODE.idx) this.navigateTo(this.SECS.length - 1);
          else this.SECS[this.SECS.length - 1].div.scrollIntoView(true);
        }
        else if ('v' == s) {
          if(window.innerHeight)
            window.scrollBy(0, window.innerHeight - 30);
          else if(document.documentElement.clientHeight)
            window.scrollBy(0, document.documentElement.clientHeight - 30);
          else
            window.scrollBy(0, document.body.cleintHeight - 30);
        }
        else if ('V' == s) {
          if(window.innerHeight)
            window.scrollBy(0, -(window.innerHeight - 30));
          else if(document.documentElement.clientHeight)
            window.scrollBy(0, -(document.documentElement.clientHeight - 30));
          else
            window.scrollBy(0, -(document.body.cleintHeight - 30));
        }
        else if ('l' == s) {
          this.warn('[[' + this.BASE_URL + '#' + this.NODE.base_id + ']['
                    + document.title + ', Sec. ' + this.removeTags(this.NODE.heading.innerHTML) + ']]',
                    true);
          this.CONSOLE_INPUT.select();
          return;
        }
        else if ('u' == s) {
          if(this.NODE.parent != this.ROOT) {
            this.NODE = this.NODE.parent;
            this.showSection(this.NODE.idx);
          }
        }
        else if ('f' == s) {
          if(this.VIEW != this.INFO_VIEW) {
            this.NODE.fold();
            this.NODE.div.scrollIntoView(true);
          }
        }
        else if ('g' == s) {
          if(this.VIEW != this.INFO_VIEW) {
            this.toggleGlobaly();
            this.NODE.div.scrollIntoView(true);
          }
        }
        else if ('?' == s || 'l' == s || '¿' == s) {
          this.showHelp();
        }
        else if ('H' == s && this.LINK_HOME) {
          window.document.location.href = this.LINK_HOME;
        }
        else if ('h' == s && this.LINK_UP) {
          window.document.location.href = this.LINK_UP;
        }
        else if ('s' == s) {
          var matches = this.SECEX.exec(window.prompt("Enter Section:"));
          var sec = matches[1];
          var sec_found = false;
          for(var i = 0; i < this.SECS.length; ++i) {
            if(this.SECS[i].base_id == sec) {
              this.pushHistory(this.SECS[i].idx, this.NODE.idx);
              this.showSection(this.SECS[i].idx);
              return;
            }
          }
          if(! sec_found) {
            this.warn("" + sec +": no such section.");
            return;
          }
        }
      }

    this.command_str = "";
    this.CONSOLE_INPUT.value = "";
    return true;
  },


  warn: function (what, pure)
  {
    if(! pure) {
      this.CONSOLE_INPUT.style.color="red";
      this.CONSOLE_INPUT.value= what + " Press any key to proceed.";
    }
    else { this.CONSOLE_INPUT.value= what; }
    if(this.VIEW != this.INFO_VIEW) {
      var v = this.CONSOLE_INPUT.cloneNode(true);
      v.style.marginTop = '0px';
      v.style.top = '0px';
      this.NODE.div.insertBefore(v, this.NODE.div.firstChild);
      v.scrollIntoView(true);
      this.MESSAGING = this.MESSAGING_INPLACE;
    } else {
      this.CONSOLE.style.marginTop = '0px';
      this.CONSOLE_INPUT.style.marginTop = '0px';
      this.CONSOLE.style.top = '0px';
      this.MESSAGING = this.MESSAGING_TOP;
    }
  },

  removeWarning: function()
  {
    if(this.MESSAGING) {
      this.CONSOLE_INPUT.value = "";
      this.CONSOLE_INPUT.style.color = "#666666";
      this.CONSOLE.style.marginTop = '-40px';
      this.CONSOLE_INPUT.style.marginTop = '-40px';
      this.CONSOLE.style.top = '-40px';
      this.command_str = "";
      this.CONSOLE_INPUT.value = "";
      if(this.MESSAGING_INPLACE == this.MESSAGING) {
        this.NODE.div.removeChild(this.NODE.div.firstChild);
      }
      this.MESSAGING = false;
      return;
    }
  },

  toggleGlobaly: function ()
  {
    if(this.ROOT.durty) {
      this.ROOT.state = OrgNode.STATE_UNFOLDED;
    }

    if(OrgNode.STATE_UNFOLDED == this.ROOT.state) {
      for(var i=0;i<this.ROOT.children.length;++i) {
        // Pretend they are unfolded. They will toggle to FOLDED then:
        this.ROOT.children[i].state = OrgNode.STATE_UNFOLDED;
        this.ROOT.children[i].fold(true);
      }
      this.ROOT.state = OrgNode.STATE_UNFOLDED;
      this.ROOT.state = OrgNode.STATE_FOLDED;
    }
    else if(OrgNode.STATE_FOLDED == this.ROOT.state) {
      for(var i=0;i<this.ROOT.children.length;++i)
        this.ROOT.children[i].fold(true);
      this.ROOT.state = OrgNode.STATE_HEADLINES;
    }
    else {
      for(var i=0;i<this.ROOT.children.length;++i)
        this.ROOT.children[i].fold();
      this.ROOT.state = OrgNode.STATE_UNFOLDED;
    }

    // All this sets ROOT durty again. So clean it:
    this.ROOT.durty = false;
  },


  showHelp: function ()
  {
    this.helping = this.helping ? 0 : 1;
    if (this.helping) {
      this.last_view_mode = this.VIEW;
      this.infoView(true);
      this.WINDOW.innerHTML = '<h2>Keyboard Shortcuts</h2>'
        +'<table cellpadding="3" style="margin:20px;border-style:none;" border="0">'
        +'<tr><td> <code><b>?/&iquest;/l</b></code> </td><td> show this help screen</td></tr>'
        +'<tr><td> <code><b>n</b></code> </td><td> goto the next section</td></tr>'
        +'<tr><td> <code><b>p</b></code> </td><td> goto the previous section</td></tr>'
        +'<tr><td> <code><b>t/&lt;</b></code> </td><td> goto the first section</td></tr>'
        +'<tr><td> <code><b>E/&gt;</b></code> </td><td> goto the last section</td></tr>'
        +'<tr><td> <code><b>i</b></code> </td><td> show table of contents</td></tr>'
        +'<tr><td> <code><b>s</b></code> </td><td> goto section</td></tr>'
        +'<tr><td> <code><b>b</b></code> </td><td> go back to last visited section. Only when following internal links.</td></tr>'
        +'<tr><td> <code><b>m</b></code> </td><td> toggle the view mode</td></tr>'
        +'<tr><td> <code><b>f</b></code> </td><td> fold current section (plain view)</td></tr>'
        +'<tr><td> <code><b>g</b></code> </td><td> fold globaly (plain view)</td></tr>'
        +'<tr><td> <code><b>l</b></code> </td><td> display org link</td></tr>'
        +'<tr><td> <code><b>v</b></code> </td><td> scroll down</td></tr>'
        +'<tr><td> <code><b>V</b></code> </td><td> scroll back up</td></tr>'
        +'<tr><td> <code><b>u</b></code> </td><td> one level up (parent section)</td></tr>'
        +'<tr><td> <code><b>h</b></code> </td><td> if supplied, go to the main index in this directory (home)</td></tr>'
        +'<tr><td> <code><b>H</b></code> </td><td> if supplied, go to link homepage (HOME)</td></tr>'
        +'</table><br />Press any key to proceed.';
    }
    else {
      if(this.PLAIN_VIEW == this.last_view_mode) {
        this.plainView();
      }
      this.showSection(this.NODE.idx);
    }
  },


 convertLinks: function ()
  {
    var i = (this.HIDE_TOC ? 0 : 1);
    for(i; i < this.SECS.length; ++i)
      {
        if(this.SECS[i].folder)
          {
            var links = this.SECS[i].folder.getElementsByTagName("a");

            for(var j=0; j<links.length; ++j) {
              var href = links[j].href.replace(this.BASE_URL, '');
              if(0 == href.indexOf('#') && links[j].href.match(this.REGEX)) {
                var matches = this.EXCHANGE.exec(href);
                if(matches) {
                  var id = matches[1].substr(4);
                  // could use quicksort like search here:
                  for(var k = 0; k < this.SECS.length; ++k) {
                    if(this.SECS[k].base_id == id) {
                      links[j].href="javascript:org_html_manager.navigateTo("+k+")";
                      break;
                    }}}}}}}
  },


  /**
   * This one is just here, because we might want to push different than
   * navigational commands on the history in the future. Is this true?
   */
  navigateTo: function (sec)
  {
    this.pushHistory(sec, this.NODE.idx);
    this.showSection(sec);
  },


  /**
   *  All undoable navigation commands should push the oposit here
   */
  pushHistory: function (command, undo)
  {
    if(! this.SKIP_HISTORY) {
      this.HISTORY[this.HIST_INDEX] = new Array(command, undo);
      this.HIST_INDEX = (this.HIST_INDEX + 1) % 50;
    }
    this.SKIP_HISTORY = false;
    this.CONSOLE_INPUT.value = "";
  },


 /**
  * only 'b' and 'B' trigger this one
  */
  popHistory: function (foreward)
  {
    if(foreward) {
      if(this.HISTORY[this.HIST_INDEX]) {
        var s = parseInt(this.HISTORY[this.HIST_INDEX][0]);
        if(! isNaN(s) || 'toc' == this.HISTORY[this.HIST_INDEX][0]) {
          this.showSection(this.HISTORY[this.HIST_INDEX][0]);
          this.CONSOLE_INPUT.value = "";
        }
        else {
          this.SKIP_HISTORY = true;
          this.CONSOLE_INPUT.value = this.HISTORY[this.HIST_INDEX][0];
          this.getKey();
        }
        this.HIST_INDEX = (this.HIST_INDEX + 1) % 50;
      }
      else
        this.warn("History: No where to foreward go from here.");
    } else {
      if(this.HISTORY[this.HIST_INDEX - 1]) {
        this.HIST_INDEX = this.HIST_INDEX == 0 ? 49 : this.HIST_INDEX - 1;
        var s = parseInt(this.HISTORY[this.HIST_INDEX][1]);
        if(! isNaN(s) || 'toc' == this.HISTORY[this.HIST_INDEX][1]) {
          this.showSection(this.HISTORY[this.HIST_INDEX][1]);
          this.CONSOLE_INPUT.value = "";
        }
        else {
          this.SKIP_HISTORY = true;
          this.CONSOLE_INPUT.value = this.HISTORY[this.HIST_INDEX][1];
          this.getKey();
        }
      }
      else
        this.warn("History: No where to back go from here.");
    }
  }

};


function OrgHtmlManagerKeyEvent (e)
{
  var c;
  if (!e) e = window.event;
  if (e.which) c = e.which;
  else if (e.keyCode) c = e.keyCode;

  if(e.ctrlKey) // || e.modifiers & Event.CONTROL_MASK)
    return;

  var s = String.fromCharCode(c);
  if(e.shiftKey) // || e.modifiers & Event.SHIFT_MASK)
    org_html_manager.CONSOLE_INPUT.value = org_html_manager.CONSOLE_INPUT.value + s;
  else
    org_html_manager.CONSOLE_INPUT.value = org_html_manager.CONSOLE_INPUT.value + s.toLowerCase();

  org_html_manager.getKey();
}


/**
 * Wait for document.body to be loaded and call org_html_manager.init().
 * In Opera document.body is true, even if not loaded (try with big big
 * file). This case is handled by the OrgHtmlManager class itself.
 */
function OrgHtmlManagerLoadCheck()
{
  org_html_manager.init();
}
