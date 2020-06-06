const express= require('express')
const mongoose = require('mongoose')
const app = express()
const url ='mongodb://localhost/NamrathaDBX' // node is connecting to the  mongoDB database

mongoose.connect(url,{useNewUrlParser:true}) // to avoid warnings if any features are deprecated
const con = mongoose.connection //to hold the connection in an object

con.on('open', function(){
    console.log('connected..')
})   //it will fire an event once connection is setup and exectute a call back function

app.use(express.json())
const memberRouter = require('./routes/members')
app.use('/members',memberRouter)
app.listen(9000,()=>{   // function() can be represented as ()=> 
    console.log('server started')
})