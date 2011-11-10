
{ name: "Next promo", inprogress: false, priority:0,  tasks : [ "select product", "add inventory", "do placement"]}

db.jobs.save( {name: "Next promo 1",inprogress: false, priority:0,tasks : [ "select product", "add inventory", "do placement"]} );
db.jobs.findAndModify({query: {inprogress: true}, sort:{}, update : {$set: {inprogress: true, started: new Date()}}, new: true} )

job = db.jobs.findAndModify( {sort:{priority:0}, remove:true, new: true} );