/*********************************************************
* All sources in Hellbound are Copyright (c) 2008 Konijn *
* I Konijn, release all code and data under the terms of *
* of the GNU General Public License (version 2), as well *
* as under the traditional Angband license. Distribution *
* is allowed both under the terms of the GPL (version 2) *
* or under the terms of the traditional Angband license. *
*********************************************************/

var _options;

//Todo !! , view_perma_grids -> name should be viewPermaGrids

function createOption( name , page , value , visible , description ){
  var option = new Object(name);
  option.name = name;
  option.page = page;
  option.value = value
  option.oldvalue = value;
  option.visible = visible
  option.description = description;
  _options[name] = option;
}


function initializeOptions(){
  _options = new Object();
  //            key              page  default visible   long description
  createOption( "maximise_mode"  , 7 , true  , false  , "Include race/class bonuses in stats" );
  createOption( "preserve_mode"  , 7 , true  , false  , "Artifacts are not lost if you never saw them" );
  createOption( "use_autoroller" , 7 , false , true   , "Stats are rolled repeatedly with minima" );
  createOption( "spend_points"   , 7 , true  , true   , "Stats are not rolled, points are spent on them" );
  createOption( "ironman_shop"   , 7 , false , true   , "Shops (except for libraries) are locked" );
  createOption( "ironman_shop"   , 7 , false , true   , "Shops (except for libraries) are locked" );

  createOption( "rand_unbiased"   , 5 , true , false   , "Remove bias from RNG" );

  createOption( "view_perma_grids"   , 1 , true , false   , "Lit squares are remembered" );

  createOption( "auto_scum"       , 3 , false , true   , "Auto scum for good levels" );
  createOption( "preserve_mode"   , 3 , true  , true   , "Auto scum for good levels" );
  
  createOption( "plain_descriptions"   , 2 , true , false   , "Show plain descriptions" );

  _options.pageHeaders = new Object();
  _options.pageHeaders[7] = "Birth Options";
}

function doOptionPage( page ){
  _options.__nextFunction = nextFunction;
  _options.page = page;
  _options.currentRow = 2;
  setMaxRow();
  nextFunction = drawOptionsScreen;
  drawOptionsScreen( 0 );
}

//Include race/class bonuses in stat calcs        : yes  (maximise_mode)

function setMaxRow(){
	var loopRow = 0;
	for( o in _options ){
		o = _options[o]
		if( o.page !== undefined ){
			p = o.page
			if( p == _options.page && o.visible){
				loopRow++;
			}
		}
	}
	//Options start on second row, and we counted one too far
	_options.maxRow = loopRow+1; 
}

function drawOptionsScreen( e ){

  processGeneralOptionsKeys( e );
  _options.currentRow = _options.currentRow + getVerticalDirection(e);
  if( _options.currentRow < 2)_options.currentRow = _options.maxRow;
  if( _options.currentRow > _options.maxRow)_options.currentRow=2;

  if( _options.cancel ){
    nextFunction = _options.__nextFunction;
    for( o in _options ){
      o = _options[o]
      if( o.oldvalue ){
        o.value = o.oldvalue;
      }
    }
    _options.cancel = false;
    nextFunction(0);
    return;
  }

  if( _options.confirm ){
    nextFunction = _options.__nextFunction;
    for( o in _options ){
      o = _options[o]
      if( o.oldvalue ){
        o.oldvalue = o.value;
      }
    }
    _options.confirm = false
    nextFunction(0);
    return;
  }

  var loopRow = 2;
  var highlightedRow = false;
  view.clear();
  view.print( _options.pageHeaders[_options.page] )
  for( o in _options ){
    o = _options[o]
    if( o.page !== undefined){
			p = o.page;
      if( p == _options.page && o.visible){
        highlightedRow = ( loopRow == _options.currentRow );
        view.setCursor( 0 , loopRow );
        view.print( ( highlightedRow?">":" " ) + o.description );
        view.setCursor( 50 , loopRow );
        o.value = ( highlightedRow && _options.toggleRequest )?!o.value:o.value;
        view.print( ": " + ( o.value?"True":"False" ) );
        loopRow++;
      }
    }
  }
  view.setCursor( 0 , 23 );
           //" ?) Help             =) Options          S) reStart          Q) Quit            "
  view.print( " ?) Help           Esc) Cancel       Space) Toggle         Ent) Confirm" )
  view.draw();
}

function processGeneralOptionsKeys(e){
  if(!e)return;
  _options.toggleRequest = ( e == 32 ); //Toggling is space
  _options.cancel        = ( e == 27 ); //Cancel is escape
  _options.confirm       = ( e == 13 ); //Confirm is enter
}
