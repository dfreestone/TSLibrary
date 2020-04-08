// Use NeDB to store the timestamp of all the recorded changes that people made
// Then we can just use load the database and reconstruct what inventory we have.


// Use a raspberry pi to record record the tempreature, humidity, lighting levels, detect power outages, and to give
//    them a token to enter to get into the system in the first place.


// log in with code and input initials at the same time.
// If there are any pressing issuues (weighing animals, cleaning cages, changing water, etc.)
//      (they can click okay, to continue)

// Then they'll see a screen with the following 5 options.
// - clean
//      - sweep
//      - dust
//      - other
// - fix (for each of these, after you say the issue, it should prompt for which box)
//        (it should also update inventory if they replaced it, although we don't need to update inventory, just check to 
//          see whether we're low)
//      - feeder
//      - lights
//      - photointerruptors
//      - other
// - health
//      - weight
//      - food (ask for spplements amount if needed.)
//      - water
//      - cages
//      - report health
// - supplies
//      - opened new box of food 
//      - request supplies
// - experiment
//      - I dont' know what should go here. (Moved to new phase? SOmething like that...)
//      - Report a problem?
// - add to inventory
//      - feeder
//      - food?
//      - lights
//      - photoreceptors
//      - other

const express = require("express");
const Datastore = require('nedb');

// initialize the data base
const eventlog_database = new Datastore({ filename: 'TSeventlog.db', autoload: true });
var passcode = null;

// initialize the server
const app = express();
app.listen(3000, () => console.log("listening at 3000"));
app.use(express.static('public'));
app.use(express.json({ limit: "1mb" }));

// post requests
// login
app.post('/api', (request, response) => {

   switch (request.body.type) {
      case 'request_passcode':
         // TODO(David): The problem with this method is that they could just stay logged in...
         //              How do I implement a timeout?
         passcode = create_passcode(5);
         console.log(passcode);
         response.json({success : true });
         break;

      case 'login_attempt':
         const { user_passcode, user } = request.body;
         result = (user_passcode == passcode) ? 'login_success' : 'login_failure';
         log({ type: result, user: user });
         response.json({ success: (user_passcode == passcode) });
         break;

         case 'user_event': 
         log(request.body);
         break;
   }
}) // app.post


function create_passcode(passcode_length) {
   // should be created new each day, and be displayed on the raspberry pi 
   let passcode = Math.random().toString(36).slice(2).slice(1, passcode_length + 1)
   eventlog_database.insert({
      timestamp: Date.now(),
      initials: "system",
      passcode: passcode
   });
   return (passcode)
}

function log(eventlog) {
    // Logs an event with the timestamp
   eventlog.timestamp = Date.now();
   eventlog_database.insert(eventlog);
}