/**
 * @file
 *       org-info.js, v.0.0.6.9
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
 *
 * The script is now roughly devided in sections by form-feeds. Editors can
 * move section wise using the common emacs commands for this purpos ('M-x ]'
 * and  'M-x ]').
 *
 * The sections are:
 *    1. This comment block.
 *    2. Everything around =OrgNodes=.
 *    3. =org_html_manager= constructor and setup.
 *    4. =org_html_manager= folding and view related stuff.
 *    5. =org_html_manager= history related methods.
 *    6. =org_html_manager= minibuffer handling.
 *    7. =org_html_manager= user input.
 *    8. =org_html_manager= search functonality.
 *    9. =org_html_manager= misc.
 *    10. Global functions.
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
  this.hasHighlight = false;              // Node highlighted (search)
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
    folder.isOrgNodeFolder = true;
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

/**
 * Find the OrgNode containing a DOM-text-node.
 * @param dom The text node.
 * @param org The OrgNode containing the OrgNode in question.
 */
OrgNode.textNodeToIdx = function (dom, org)
{
  while(dom.nodeType !=  1 /* IE has no Node.ELEMENT_NODE... */
        || -1 == dom.attributes["id"].value.indexOf("outline-container-")) {
    dom = dom.parentNode;
  }
  var base_id = dom.attributes["id"].value.substr(18);
  return OrgNode.idxForBaseId(base_id, org);
};

/**
 * Find an OrgNode with base_id inside an OrgNode and return it's idx.
 * @param base The base_id.
 * @param org The OrgNode.
 */
OrgNode.idxForBaseId = function(base, org)
{
  if(org.base_id == base) return org;
  for(var i = 0; i < org.children.length; ++i) {
    var o = OrgNode.idxForBaseId(idx, org.children[i]);
    if(null != o) return o;
  }
  return null;
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
  // else
  //   alert("folder == null\nCheck your org export version.");
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
  STARTUP_MESSAGE: 0,          // Show info at startup?

  // Private
  BASE_URL: document.URL,      // URL without '#sec-x.y.z'
  START_SECTION: 0,            // Will be evtl. recomputed from '#sec-x.y.z'
  ROOT: null,                  // Root element or our OrgNode tree
  NODE: null,                  // The current node
  TITLE: null,                 // Save the title for hide/show
  LOAD_CHECK: null,            // Saves the setTimeout()'s value
  WINDOW: null,                // A div to display info view mode
  SECS: new Array(),           // The OrgNode tree
  REGEX: /(#sec\-)(.*$)/, // identify a section link in toc
  EXCHANGE: /(sec-.*)$/,  // extract the section number
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
  CONSOLE_LABEL: null,
  CONSOLE_OFFSET: "50px",
  OCCUR: "",                   // The search string.
  SEARCH_REGEX: "",
  SEARCH_HL_REG: new RegExp('>([^<]*)?(<span [^>]*?"org-info-js_search-highlight"[^>]*?>)(.*?)(<\/span>)([^>]*)?<', "ig"),
  console_first_time: true,    // Cookie would be cool maybe.
  MESSAGING: 0,                // Is there a message in the console?
  MESSAGING_INPLACE: 1,
  MESSAGING_TOP: 2,
  // show help:
  HELPING: false,
  // console in read mode?
  READING: false,
  // if yes, which command caused the readmode?
  READ_COMMAND: "",
  // numerical commands are internal commands.
  READ_COMMAND_NULL: "_0",
  READ_COMMAND_HTML_LINK: "_1",
  READ_COMMAND_ORG_LINK: "_2",
  LAST_WAS_SEARCH: false,      // if this is true, and OCCUR unchanged, skip to next section if repeated search.
  last_view_mode:0,
  TAB_INDEX: 1000,             // Users could have defined tabindexes!
  SEARCH_HIGHLIGHT_ON: false,

  /**
   * Setup the OrgHtmlManager for scanning.
   * Here the timeout func gets set, that tells the wakes up org_html_mager
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
          case 'HELP':
          case 'VIEW':
          case 'HIDE_TOC':
          case 'LOCAL_TOC':
          case 'VIEW':
          case 'OCCUR':
            this.set(k, decodeURIComponent(v));
            break;
          default: break;
          }
        }
      }
    }
    this.VIEW  = this.VIEW ? this.VIEW : this.PLAIN_VIEW;
    this.VIEW_BUTTONS = (this.VIEW_BUTTONS && this.VIEW_BUTTONS != "0") ? true : false;
    this.STARTUP_MESSAGE = (this.STARTUP_MESSAGE && this.STARTUP_MESSAGE != "0") ? true : false;
    this.LOCAL_TOC = (this.LOCAL_TOC && this.LOCAL_TOC != "0") ? true : false;
    this.HIDE_TOC = (this.TOC && this.TOC != "0") ? false : true;
    if(this.FIXED_TOC && this.FIXED_TOC != "0") {
      this.FIXED_TOC = true;
      this.HIDE_TOC = false;
    }
    else this.FIXED_TOC = false;

    this.LINKS +=
    ((this.LINK_UP && this.LINK_UP != document.URL) ? '<a href="'+this.LINK_UP+'">Up</a> / ' : "") +
    ((this.LINK_HOME && this.LINK_HOME != document.URL) ? '<a href="'+this.LINK_HOME+'">HOME</a> / ' : "") +
    '<a onclick="org_html_manager.showHelp();">HELP</a> / ';

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
    this.WINDOW.style.marginBottom = "40px";
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
    }
    if(-1 != this.BASE_URL.indexOf('?'))
      this.BASE_URL = this.BASE_URL.substring(0, this.BASE_URL.indexOf('?'));
    if(-1 != this.BASE_URL.indexOf('#'))
      this.BASE_URL = this.BASE_URL.substring(0, this.BASE_URL.indexOf('#'));

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
      // CANCELED: warn if not scanned_all
    }

    this.CONSOLE = document.createElement("div");
    this.CONSOLE.innerHTML = '<form action="" style="margin:0px;padding:0px;" onsubmit="org_html_manager.evalReadCommand(); return false;">'
      +'<table id="org-info-js_console" style="width:100%;margin:0px 0px 0px 0px;" cellpadding="0" cellspacing="0" summary="minibuffer">'
      +'<tbody><tr><td id="org-info-js_console-icon" style="padding:0px 0px 0px 0px;">&#160;</td><td style="width:100%;vertical-align:middle;padding:0px 0px 0px 0px;">'
      +'<table style="width:100%;margin:0px 0px 0px 0px;" cellpadding="0" cellspacing="2">'
      +'<tbody><tr><td id="org-info-js_console-label" style="white-space:nowrap;padding:0px 0px 0px 0px;"></td></tr>'
      +'<tr><td style="width:100%;vertical-align:middle;padding:0px 0px 0px 0px;">'
      +'<input type="text" id="org-info-js_console-input" onkeydown="org_html_manager.getKey();"'
      +' onclick="this.select();" maxlength="150" style="width:100%;padding:0px;margin:0px 0px 0px 0px;border-style:none;"'
      +' value=""/></td></tr></tbody></table></td><td style="padding:0px 0px 0px 0px;">&#160;</td></tr></tbody></table>'
      +'</form>';
    this.CONSOLE.style.position = 'relative';
    this.CONSOLE.style.marginTop =  "-" + this.CONSOLE_OFFSET;
    this.CONSOLE.style.top = "-" + this.CONSOLE_OFFSET;
    this.CONSOLE.style.left = '0px';
    this.CONSOLE.style.width = '100%';
    this.CONSOLE.style.height = '40px';
    this.CONSOLE.style.overflow = 'hidden';
    this.CONSOLE.style.verticalAlign = 'middle';
    this.CONSOLE.style.zIndex = '9';
    this.CONSOLE.style.border = "1px solid #cccccc";
    this.CONSOLE.id = 'org-info-js_console-container';

    document.body.insertBefore(this.CONSOLE, document.body.firstChild);
    this.MESSAGING = false;
    this.CONSOLE_LABEL = document.getElementById("org-info-js_console-label");
    this.CONSOLE_INPUT = document.getElementById("org-info-js_console-input");
    document.onkeypress=OrgHtmlManagerKeyEvent;

    if("" != this.OCCUR) {
      this.CONSOLE_INPUT.value = this.OCCUR;
      this.READ_COMMAND = 'o';
      this.evalReadCommand();
    }

    if(0 != this.DEBUG && this.debug.length) alert(this.debug);
    if(this.STARTUP_MESSAGE) {
      this.warn("This page uses org-info.js. Press '?' for more information.", true);
    }
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
        heading.style.cursor = "pointer";
        if(this.MOUSE_HINT) {
          heading.onmouseover = function(){org_html_manager.highlight_headline(0);};
          heading.onmouseout = function(){org_html_manager.unhighlight_headline(0);};
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
      // So we stop scanning and set the timeout func in caller.
      // We could even count the <ul> and <li> elements above to speed up the next
      // scan.
      if(null == heading) {
        this.debug += ("heading is null. Scanning stopped.\n");
        return(false);
      }
      var div = heading.parentNode;
      var sec = this.SECS.length;
      var depth = div.className.substr(8);
      var id = matches[1].substr(4);
      heading.onclick = function() {org_html_manager.fold("" + sec);};
      heading.style.cursor = "pointer";
      if(this.MOUSE_HINT) {
        heading.onmouseover = function() {org_html_manager.highlight_headline("" + sec);};
        heading.onmouseout = function() {org_html_manager.unhighlight_headline("" + sec);};
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
      var html = '<table class="org-info-js_info-navigation" width="100%" border="0" style="border-bottom:1px solid black;">'
        +'<tr><td colspan="3" style="text-align:left;border-style:none;vertical-align:bottom;">'
        +'<span style="float:left;display:inline;text-align:left;">'
        +'Top: <a accesskey="i" href="javascript:org_html_manager.navigateTo(0);">'+index_name+'</a></span>'
        +'<span style="float:right;display:inline;text-align:right;font-size:70%;">'
        + this.LINKS
        +'<a accesskey="t" href="javascript:org_html_manager.toggleView('+i+');">toggle view</a></span>'
        +'</td></tr><tr><td style="text-align:left;border-style:none;vertical-align:bottom;width:22%">';

      if(i>0)
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

  /**
   * Execute arbitrary JavaScript code. Used for configuration.
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
    else if("HELP" == eval_key)
      eval("this.STARTUP_MESSAGE="+eval_val+";");

    if(eval_val)
      eval("this."+eval_key+"='"+eval_val+"';");
    else
      eval("this."+eval_key+"=0;");
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
    if(this.NODE.idx == 0) window.scrollTo(0, 0);
    else this.NODE.div.scrollIntoView(true);
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





  /**
   * This one is just here, because we might want to push different than
   * navigational commands on the history in the future. Is this true?
   */
  navigateTo: function (sec)
  {
    if     (this.READING)   { this.endRead(); }
    else if(this.MESSAGING) { this.removeWarning(); }
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
  },





  warn: function (what, harmless, value)
  {
    if(null == value) value = "";
    this.CONSOLE_INPUT.value = value;
    if(! harmless) this.CONSOLE_LABEL.style.color = "red";
    this.CONSOLE_LABEL.innerHTML = "<span style='float:left;'>"+what +"</span>"+
    "<span style='float:right;color:#aaaaaa;font-weight:normal;'>(press any key to proceed)</span>";
    this.showConsole();
    // wait until keyup was processed:
    window.setTimeout(function(){org_html_manager.CONSOLE_INPUT.value=value;}, 50);
  },

  startRead: function (command, label, value, shortcuts)
  {
    if(null == value) value = "";
    if(null == shortcuts) shortcuts = "";
    this.READ_COMMAND = command;
    this.READING = true;
    this.CONSOLE_LABEL.innerHTML = "<span style='float:left;'>"+label+"</span>"+
    "<span style='float:right;color:#aaaaaa;font-weight:normal;'>("+shortcuts+"RET to close)</span>";
    this.showConsole();
    document.onkeypress=null;
    this.CONSOLE_INPUT.focus();
    this.CONSOLE_INPUT.onblur = function() {org_html_manager.CONSOLE_INPUT.focus();};
    // wait until keyup was processed:
    window.setTimeout(function(){org_html_manager.CONSOLE_INPUT.value=value;}, 50);
  },

  endRead: function (command, label)
  {
    this.READING = false;
    this.READ_COMMAND = "";
    this.CONSOLE_INPUT.onblur = null;
    this.CONSOLE_INPUT.blur();
    document.onkeypress=OrgHtmlManagerKeyEvent;
    // this.hideConsole();
  },

  removeWarning: function()
  {
    this.CONSOLE_LABEL.style.color = "#333333";
    this.hideConsole();
  },

  showConsole: function()
  {
    if(!this.MESSAGING) {
      if(this.VIEW != this.INFO_VIEW) {
        // Maybe clone the CONSOLE?
        document.body.removeChild(document.body.firstChild);
        this.NODE.div.insertBefore(this.CONSOLE, this.NODE.div.firstChild);
        this.NODE.div.scrollIntoView(true);
        this.MESSAGING = this.MESSAGING_INPLACE;
      } else {
        this.MESSAGING = this.MESSAGING_TOP;
        window.scrollTo(0, 0);
      }
      this.CONSOLE.style.marginTop = '0px';
      this.CONSOLE.style.top = '0px';
    }
  },

  hideConsole: function()
  {
    if(this.MESSAGING) {
      this.CONSOLE.style.marginTop = "-" + this.CONSOLE_OFFSET;
      this.CONSOLE.style.top = "-" + this.CONSOLE_OFFSET;
      this.CONSOLE_LABEL.innerHTML = "";
      this.CONSOLE_INPUT.value = "";
      if(this.MESSAGING_INPLACE == this.MESSAGING) {
        this.NODE.div.removeChild(this.NODE.div.firstChild);
        document.body.insertBefore(this.CONSOLE, document.body.firstChild);
        if(this.NODE.idx != 0) this.NODE.div.scrollIntoView();
      }
      this.MESSAGING = false;
    }
  },





  /**
   * All commands that add something to the history should return.
   */
  getKey: function ()
  {
    var s = this.CONSOLE_INPUT.value;
    // return, if s is empty:
    if(0 == s.length) {
      if(this.MESSAGING && !this.READING) this.removeWarning();
        return;
    }

    // the easiest is to just drop everything and clean the console.
    // User has to retype again.
    if(this.MESSAGING && !this.READING) {
      this.removeWarning();
      return;
    }
    else if(this.HELPING) {
      this.showHelp();
      this.CONSOLE_INPUT.value = "";
      return;
    }
    else if(this.READING) {
      return;
    }

    this.CONSOLE_INPUT.blur();

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
        if ('b' == s) {
          this.popHistory();
        }
        else if ('B' == s) {
          this.popHistory(true);
        }
        else if ('c' == s) {
          this.removeSearchHighlight();
          if(this.VIEW == this.INFO_VIEW) {
            // redisplay in info view mode:
            this.showSection(this.NODE.idx);
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
        else if ('n' == s) {
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
        else if ('q' == s) {
          if(window.confirm("Really close this file?")) {
            window.close();
          }
        }
        else if ('<' == s || 't' == s) {
          if(0 != this.NODE.idx) this.navigateTo(0);
          else window.scrollTo(0,0);
        }
        else if ('>' == s || 'E' == s || 'e' == s) {
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
        else if ('F' == s) {
          if(this.VIEW != this.INFO_VIEW) {
            this.toggleGlobaly();
            this.NODE.div.scrollIntoView(true);
          }
        }
        else if ('?' == s || '¿' == s) {
          this.showHelp();
        }
        else if ('H' == s && this.LINK_HOME) {
          window.document.location.href = this.LINK_HOME;
        }
        else if ('h' == s && this.LINK_UP) {
          window.document.location.href = this.LINK_UP;
        }

        /* === READ COMMANDS === */

        else if ('l' == s) {
          if("" != this.OCCUR) {
            this.startRead(this.READ_COMMAND_HTML_LINK, "Choose HTML-link type: 's' = section, 'o' = occur");
          } else {
            this.startRead(s, "HTML-link:",
                           '<a href="' + this.BASE_URL + "#sec-" + this.NODE.base_id + '">' +
                           document.title + ", Sec. '" + this.removeTags(this.NODE.heading.innerHTML) + "'</a>",
                           "C-c to copy, ");
            window.setTimeout(function(){org_html_manager.CONSOLE_INPUT.select();}, 100);
          }
          return;
        }
        else if ('L' == s) {
          if("" != this.OCCUR) {
            this.startRead(this.READ_COMMAND_ORG_LINK, "Choose Org-link type: 's' = section, 'o' = occur");
          } else {
            this.startRead(s, "Org-link:",
                           '[[' + this.BASE_URL + "#sec-" + this.NODE.base_id + '][' +
                           document.title + ", Sec. '" + this.removeTags(this.NODE.heading.innerHTML) + "']]",
                           "C-c to copy, ");
            window.setTimeout(function(){org_html_manager.CONSOLE_INPUT.select();}, 100);
          }
          return;
        }
        else if ('g' == s) {
          this.startRead(s, "Enter section number:");
          return;
        }
        else if ('o' == s) {
          if("" != this.OCCUR) this.startRead(s, "Occur:", this.OCCUR, "RET to use previous, DEL ");
          else this.startRead(s, "Occur:", this.OCCUR);
          window.setTimeout(function(){org_html_manager.CONSOLE_INPUT.value=org_html_manager.OCCUR;org_html_manager.CONSOLE_INPUT.select();}, 100);
          return;
        }
        else if ('s' == s) {
          if("" != this.OCCUR) this.startRead(s, "Search forward:", this.OCCUR, "RET to use previous, DEL ");
          else this.startRead(s, "Search forward:", this.OCCUR);
          window.setTimeout(function(){org_html_manager.CONSOLE_INPUT.value=org_html_manager.OCCUR;org_html_manager.CONSOLE_INPUT.select();}, 100);
          return;
        }
        else if ('S' == s) {
          if("" == this.OCCUR) {
            s = "s";
            this.startRead(s, "Search forward:");
          }
          else {
            this.READ_COMMAND = s;
            this.evalReadCommand();
          }
          return;
        }
        else if ('r' == s) {
          if("" != this.OCCUR) this.startRead(s, "Search backwards:", this.OCCUR, "RET to use previous, DEL ");
          else this.startRead(s, "Search backwards:", this.OCCUR);
          window.setTimeout(function(){org_html_manager.CONSOLE_INPUT.value=org_html_manager.OCCUR;org_html_manager.CONSOLE_INPUT.select();}, 100);
          return;
        }
        else if ('R' == s) {
          if("" == this.OCCUR) {
            s = "r";
            this.startRead(s, "Search backwards:");
          }
          else {
            this.READ_COMMAND = s;
            this.evalReadCommand();
          }
          return;
        }
      }

    this.CONSOLE_INPUT.value = "";
    return;
  },

  /**
   * Please return, if you want the minibuffer to stay on screen.
   * Remember to call this.endRead()!
   */
  evalReadCommand: function()
  {
    var command = this.READ_COMMAND;
    var result  = this.CONSOLE_INPUT.value;

    this.endRead();

    if("" == command || "" == result) {
      this.hideConsole();
      return;
    }

    if(command == 'g') { // goto section
      var matches = this.SECEX.exec(result);
      var sec = matches[1];
      var sec_found = false;
      for(var i = 0; i < this.SECS.length; ++i) {
        if(this.SECS[i].base_id == sec) {
          this.hideConsole();
          this.navigateTo(this.SECS[i].idx);
          return;
        }
      }
      if(! sec_found) {
        this.warn("Goto section: no such section.", false, sec);
        return;
      }
    }

    else if(command == 's') { // search
      if("" == result) return false;
      if(this.SEARCH_HIGHLIGHT_ON) this.removeSearchHighlight();
      var restore = this.OCCUR;
      var plus = 0;
      if(result == this.OCCUR) plus++;
      this.OCCUR = result;
      this.makeSearchRegexp();
      for(var i = this.NODE.idx + plus; i < this.SECS.length; ++i) {
        if(this.searchTextInOrgNode(i)) {
          this.OCCUR = result;
          this.hideConsole();
          this.navigateTo(this.SECS[i].idx);
          return;
        }
      }
      this.warn("Search forwards: text not found.", false, this.OCCUR);
      this.OCCUR = restore;
      return;
    }

    else if(command == 'S') { // repeat search
      for(var i = this.NODE.idx + 1; i < this.SECS.length; ++i) {
        if(this.searchTextInOrgNode(i)) {
          this.hideConsole();
          this.navigateTo(this.SECS[i].idx);
          return;
        }
      }
      this.warn("Search forwards: text not found.", false, this.OCCUR);
      return;
    }

    else if(command == 'r') { // search backwards
      if("" == result) return false;
      if(this.SEARCH_HIGHLIGHT_ON) this.removeSearchHighlight();
      var restore = this.OCCUR;
      this.OCCUR = result;
      var plus = 0;
      if(result == this.OCCUR) plus++;
      this.makeSearchRegexp();
      for(var i = this.NODE.idx - plus; i > -1; --i) {
        if(this.searchTextInOrgNode(i)) {
          this.hideConsole();
          this.navigateTo(this.SECS[i].idx);
          return;
        }
      }
      this.warn("Search backwards: text not found.", false, this.OCCUR);
      this.OCCUR = restore;
      return;
    }

    else if(command == 'R') { // repeat search backwards
      for(var i = this.NODE.idx - 1; i > -1; --i) {
        result = this.removeTags(this.SECS[i].heading.innerHTML);
        if(this.searchTextInOrgNode(i)) {
          this.hideConsole();
          this.navigateTo(this.SECS[i].idx);
          return;
        }
      }
      this.warn("Search backwards: text not found.", false, this.OCCUR);
      return;
    }

    else if(command == 'o') { // occur
      if("" == result) return false;
      if(this.SEARCH_HIGHLIGHT_ON) this.removeSearchHighlight();
      var restore = this.OCCUR;
      this.OCCUR = result;
      this.makeSearchRegexp();
      var occurs = new Array();
      for(var i = 0; i < this.SECS.length; ++i) {
        if(this.searchTextInOrgNode(i)) {
          occurs.push(i);
        }
      }
      if(0 == occurs.length) {
        this.warn("Occur: text not found.", false, this.OCCUR);
        this.OCCUR = restore;
        return;
      }

      this.hideConsole();
      if(this.PLAIN_VIEW != this.VIEW) this.plainView();
      this.ROOT.durty = true;
      this.toggleGlobaly();
      for(var i = 0; i < this.SECS.length; ++i) {
        OrgNode.showElement(this.SECS[i].div);
        OrgNode.hideElement(this.SECS[i].folder);
      }
      for(var i = (occurs.length - 1); i >= 1; --i) {
        OrgNode.showElement(this.SECS[occurs[i]].folder);
      }
      this.showSection(occurs[0]);
    }

    else if(command == this.READ_COMMAND_ORG_LINK) {
      var c = result.charAt(0);
      if('s' == c) {
        this.startRead(this.READ_COMMAND_NULL, "Org-link to this section:",
                       '[[' + this.BASE_URL + "#sec-" + this.NODE.base_id + '][' +
                       document.title + ", Sec. '" +  this.removeTags(this.NODE.heading.innerHTML) + "']]",
                       "C-c to copy, ");
        window.setTimeout(function(){org_html_manager.CONSOLE_INPUT.select();}, 100);
      } else if('o' == c) {
        this.startRead(this.READ_COMMAND_NULL, "Org-link, occurences of <i>&quot;"+this.OCCUR+"&quot;</i>:",
                       '[[' + this.BASE_URL + "?OCCUR=" + this.OCCUR + '][' +
                       document.title + ", occurences of '" + this.OCCUR + "']]",
                       "C-c to copy, ");
        window.setTimeout(function(){org_html_manager.CONSOLE_INPUT.select();}, 100);
      } else {
        this.warn(c + ": No such link type!");
      }
    }

    else if(command == this.READ_COMMAND_HTML_LINK) {
      var c = result.charAt(0);
      if('s' == c) {
        this.startRead(this.READ_COMMAND_NULL, "HTML-link to this section:",
                       '<a href="' + this.BASE_URL + "#sec-" + this.NODE.base_id + '">' +
                       document.title + ", Sec. '" +  this.removeTags(this.NODE.heading.innerHTML) + "'</a>",
                       "C-c to copy, ");
        window.setTimeout(function(){org_html_manager.CONSOLE_INPUT.select();}, 100);
      } else if('o' == c) {
        this.startRead(this.READ_COMMAND_NULL, "HTML-link, occurences of <i>&quot;"+this.OCCUR+"&quot;</i>:",
                       '<a href="' + this.BASE_URL + "?OCCUR=" + this.OCCUR + '">' +
                       document.title + ", occurences of '" + this.OCCUR + "'</a>",
                       "C-c to copy, ");
        window.setTimeout(function(){org_html_manager.CONSOLE_INPUT.select();}, 100);
      } else {
        this.warn(c + ": No such link type!");
      }
    }

  },





  makeSearchRegexp: function()
  {
    var tmp = this.OCCUR.replace(/>/g, "&gt;").replace(/</g, "&lt;").replace(/=/g, "\\=").replace(/\\/g, "\\\\").replace(/\?/g, "\\?").replace(/\*/g, "\\*").replace(/\+/g, "\\+").replace(/\"/g, "&quot;");
    this.SEARCH_REGEX =  new RegExp(">([^<]*)?("+tmp+")([^>]*)?<","ig");
  },

  searchTextInOrgNode: function(i)
  {
    var ret = false;
    if(null != this.SECS[i]) {
      if(this.SEARCH_REGEX.test(this.SECS[i].heading.innerHTML)) {
        ret = true;
        this.setSearchHighlight(this.SECS[i].heading);
        this.SECS[i].hasHighlight = true;
        this.SEARCH_HIGHLIGHT_ON = true;
      }
      if(this.SEARCH_REGEX.test(this.SECS[i].folder.innerHTML)) {
        ret = true;
        this.setSearchHighlight(this.SECS[i].folder);
        this.SECS[i].hasHighlight = true;
        this.SEARCH_HIGHLIGHT_ON = true;
      }
      return ret;
    }
    return false;
  },

  setSearchHighlight: function(dom)
  {
    var tmp = dom.innerHTML;
    dom.innerHTML = tmp.replace(this.SEARCH_REGEX,
      '>$1<span class="org-info-js_search-highlight">$2</span>$3<');
  },

  removeSearchHighlight: function()
  {
    for(var i = 0; i < this.SECS.length; ++i) {
      if(this.SECS[i].hasHighlight) {
        while(this.SEARCH_HL_REG.test(this.SECS[i].heading.innerHTML)) {
          var tmp = this.SECS[i].heading.innerHTML;
          this.SECS[i].heading.innerHTML = tmp.replace(this.SEARCH_HL_REG, '>$1$3$5<');
        }
        while(this.SEARCH_HL_REG.test(this.SECS[i].folder.innerHTML)) {
          var tmp = this.SECS[i].folder.innerHTML;
          this.SECS[i].folder.innerHTML = tmp.replace(this.SEARCH_HL_REG, '>$1$3$5<');
        }
        this.SECS[i].hasHighlight = false;
      }
    }
    this.SEARCH_HIGHLIGHT_ON = false;
  },





  highlight_headline: function(h)
  {
    var i = parseInt(h);
    if(this.PLAIN_VIEW == this.VIEW && this.MOUSE_HINT) {
      if('underline' == this.MOUSE_HINT)
        this.SECS[i].heading.style.borderBottom = "1px dashed #666666";
      else
        this.SECS[i].heading.style.backgroundColor = this.MOUSE_HINT;
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

  showHelp: function ()
  {
    if     (this.READING)   { this.endRead(); }
    else if(this.MESSAGING) { this.removeWarning(); }
    /* This is an OrgMode version of the table. Turn on orgtbl-mode in
       this buffer, edit the table, then press C-c C-c with the cursor
       in the table.  The table will then be translated an inserted below.
#+ORGTBL: SEND Shortcuts orgtbl-to-generic :splice t :skip 2 :lstart "\t+'<tr>" :lend "</tr>'" :fmt (1 "<td><code><b>%s</b></code></td>" 2 "<td>%s</td>") :hline "\t+'</tbody><tbody>'"
      | Key          | Function                                                             |
      |--------------+----------------------------------------------------------------------|
      | ? / &iquest; | show this help screen                                                |
      |--------------+----------------------------------------------------------------------|
      |              | <b>Moving around</b>                                                 |
      | n / p        | goto the next / previous section                                     |
      | t / E        | goto the first / last section                                        |
      | g            | goto section...                                                      |
      | u            | go one level up (parent section)                                     |
      | i            | show table of contents                                               |
      | b / B        | go back to last / forward to next visited section.                   |
      | h / H        | go to main index in this directory / link HOME page                  |
      |--------------+----------------------------------------------------------------------|
      |              | <b>View</b>                                                          |
      | m            | toggle the view mode between info and plain                          |
      | f / F        | fold current section / whole document (plain view only)              |
      |--------------+----------------------------------------------------------------------|
      |              | <b>Searching</b>                                                     |
      | s / r        | search forward / backward....                                        |
      | S / R        | search again forward / backward                                      |
      | o            | occur-mode                                                           |
      |--------------+----------------------------------------------------------------------|
      |              | <b>Misc</b>                                                          |
      | l / L        | display HTML link / Org link                                         |
      | v / V        | scroll down / up                                                     |
      */
    this.HELPING = this.HELPING ? 0 : 1;
    if (this.HELPING) {
      this.last_view_mode = this.VIEW;
      this.infoView(true);
      this.WINDOW.innerHTML = 'Press any key or <a onclick="org_html_manager.showHelp();">click here</a> to proceed.'
        +'<h2>Keyboard Shortcuts</h2>'
        +'<table cellpadding="3" rules="groups" frame="hsides" style="margin:20px;border-style:none;" border="0";>'
	+'<tbody>'
      // BEGIN RECEIVE ORGTBL Shortcuts
	+'<tr><td><code><b>? / &iquest;</b></code></td><td>show this help screen</td></tr>'
	+'</tbody><tbody>'
	+'<tr><td><code><b></b></code></td><td><b>Moving around</b></td></tr>'
	+'<tr><td><code><b>n / p</b></code></td><td>goto the next / previous section</td></tr>'
	+'<tr><td><code><b>t / E</b></code></td><td>goto the first / last section</td></tr>'
	+'<tr><td><code><b>g</b></code></td><td>goto section...</td></tr>'
	+'<tr><td><code><b>u</b></code></td><td>go one level up (parent section)</td></tr>'
	+'<tr><td><code><b>i</b></code></td><td>show table of contents</td></tr>'
	+'<tr><td><code><b>b / B</b></code></td><td>go back to last / forward to next visited section.</td></tr>'
	+'<tr><td><code><b>h / H</b></code></td><td>go to main index in this directory / link HOME page</td></tr>'
	+'</tbody><tbody>'
	+'<tr><td><code><b></b></code></td><td><b>View</b></td></tr>'
	+'<tr><td><code><b>m</b></code></td><td>toggle the view mode between info and plain</td></tr>'
	+'<tr><td><code><b>f / F</b></code></td><td>fold current section / whole document (plain view only)</td></tr>'
	+'</tbody><tbody>'
	+'<tr><td><code><b></b></code></td><td><b>Searching</b></td></tr>'
	+'<tr><td><code><b>s / r</b></code></td><td>search forward / backward....</td></tr>'
	+'<tr><td><code><b>S / R</b></code></td><td>search again forward / backward</td></tr>'
	+'<tr><td><code><b>o</b></code></td><td>occur-mode</td></tr>'
	+'</tbody><tbody>'
	+'<tr><td><code><b></b></code></td><td><b>Misc</b></td></tr>'
	+'<tr><td><code><b>l / L</b></code></td><td>display HTML link / Org link</td></tr>'
	+'<tr><td><code><b>v / V</b></code></td><td>scroll down / up</td></tr>'
      // END RECEIVE ORGTBL Shortcuts
       +'</tbody>'
       +'</table><br />Press any key or <a onclick="org_html_manager.showHelp();">click here</a> to proceed.';
      window.scrollTo(0, 0);
    }
    else {
      if(this.PLAIN_VIEW == this.last_view_mode) {
        this.plainView();
      }
      this.showSection(this.NODE.idx);
    }
  }

};





function OrgHtmlManagerKeyEvent (e)
{
  var c;
  if (!e) e = window.event;
  if (e.which) c = e.which;
  else if (e.keyCode) c = e.keyCode;

  if(e.ctrlKey) return;

  var s = String.fromCharCode(c);
  if(e.shiftKey)
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
