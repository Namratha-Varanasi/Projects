const express = require('express')
const router = express.Router()
const Member = require('../models/member')

router.get('/', async(req,res) => {
    try{
        const members= await Member.find()
        res.json(members)
    }catch(err){
        res.send('error' + err)
    }

})


router.get('/:id', async(req,res) => {
    try{
        const member= await Member.findById(req.params.id)
        res.json(member)
    }catch(err){
        res.send('error' + err)
    }

})
router.post('/',async(req,res)=>{
    const member = new Member({
    name: req.body.name,
    tech: req.body.tech,
    sub: req.body.sub
    })
    try{
       const m1 = await member.save()
        res.json(m1)
    }catch(err){
        console.log("not executed")
        res.send('Error')
    }
})
router.patch('/:id',async(req,res) =>{
    try{
        const member = await Member.findById(req.params.id)
        member.sub=req.body.sub
        const m1= await member.save()
        res.send(m1)
    }catch(err)
    {
        res.send('error')
    }
})
router.delete('/:id',async(req,res) =>{
    try{
        const member = await Member.findById(req.params.id)
        member.sub=req.body.sub
        const m1= await member.delete()
        res.send("deleted")
    }catch(err)
    {
        res.send('error')
    }
})

module.exports = router