/*********************************************************
* All sources in Hellbound are Copyright (c) 2008 Konijn *
* I Konijn, release all code and data under the terms of *
* of the GNU General Public License (version 2), as well *
* as under the traditional Angband license. Distribution *
* is allowed both under the terms of the GPL (version 2) *
* or under the terms of the traditional Angband license. *
*********************************************************/

/*********************************************************
* All sources here are public domain from                *
* developer.mozilla.org                                  *
*********************************************************/


if (!Array.prototype.indexOf)
{
  Array.prototype.indexOf = function(elt /*, from*/)
  {
    var len = this.length;

    var from = Number(arguments[1]) || 0;
    from = (from < 0)
         ? Math.ceil(from)
         : Math.floor(from);
    if (from < 0)
      from += len;

    for (; from < len; from++)
    {
      if (from in this &&
          this[from] === elt)
        return from;
    }
    return -1;
  };
}

/*********************************************************
* All sources here are LGPL from                         *
* JQuery                                                 *
*********************************************************/








/*********************************************************
* All sources here are GPL from                          *
* Konijn                                                 *
*********************************************************/

//indexObject
//Takes all properties and makes a numerical index for them
//Store the key of the property in the indexed value if that value is an object
function indexObject( a ){

  var ta = new Array();
  var counter = 0;

  for( property in a ){
    var namedProperty = a[property];
    if( typeof namedProperty == "object")
      namedProperty._key = property;
    ta.push( namedProperty );
  }

  for( property in ta ){
    a[property] = ta[property];
  }
  
}

//cloneKeyedObject
//Creates a new object based on the key in order to not have to maintain a name property
//Copies over all properties
function cloneKeyedObject( key , o ){

  var c = new Object( key );

  for( p in o ){
    c[p] = o[p];
  }

  return c;
}

function loadScript( scriptName ){
  ta = document.hellband.loadedScripts || new Array();
  if( ta.indexOf( scriptName ) == -1 ){ 
    script = document.createElement( "SCRIPT" );
    script.src = scriptName;
    document.body.appendChild( script );
    ta.push( scriptName );
    document.hellband.loadedScripts = ta;
  }
}

var loadScriptCallBackFunction;
function loadScriptWithCallback( scriptName , callback ){
  loadScript( scriptName );
  loadScriptCallBackFunction = callback;
}


//reverse for Strings, every string should be reversable ;]
if (!String.prototype.reverse){
  String.prototype.reverse = function(){
    return this.split("").reverse().join("")
  }
}

