var d = require( "../../web/datascript.min.js" );
var PouchDB = require( "pouchdb" );
var db = new PouchDB( "http://test:test@localhost:5984/large/" );

var searchPouchIndex = function( db, index ) {
  return function( search, callback ) {
    var view = index + "/" + index;
    var endkey = search.map( function( el ) {
      if ( el === null ) {
        return {};
      }
      return el;
    });
    db.query( view, {
      startkey: search,
      endkey: endkey
    }, function( error, data ) {
      callback( data.rows.map( function( el ) {
        return el.value;
      }) );
    })
  }
};

var initPouchDB = function() {
  return d.db( db, searchPouchIndex( db, "eav" ), searchPouchIndex( db, "ave" ) );
};

var datalog = initPouchDB();
d.q( '[:find ?id :in :where [?id "last_name" "benson"]]', function( data ) {
  if ( JSON.stringify( data ) === JSON.stringify( [["h-J3zsqvJU"],["h-KNsH97zd"],["h-KYgWG520"],["h-okf5vK7R"],["h-PMx7u50O"],["h-QHXBZ9vr"]] ) ) {
    console.log( "Passed!" );
  } else {
    console.log( "Failed.", data );
  }
}, datalog );
