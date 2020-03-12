var contractSource = `include "List.aes"
payable contract LocalEventContract =

    record local_event = {
        index : int,
        owner : address,
        name : string,
        location : string,
        cover    : string,
        price  :int,
        description : string,
        paid   :int,
        up_vote   : int,
        down_vote :int,
        total_paid : int,
        history :list(users_transaction)
        }

    record users_transaction = {
        nick_name :string,
        txt_id : int,
        event_address:address,
        txt_ownwer : address,
        amount :int
        }

    record state = {
        local_events : map(int, local_event), 
        //users_transactions : map(int ,users_transaction),
        event_length : int,
        history_length : int
        }


    stateful entrypoint init() = { 
        event_length = 0,
        history_length = 0,
        local_events = {}
        //users_transactions = {}
        }
    
    
    stateful entrypoint add_event(name': string, location': string,cover' : string,price' : int,description' :string) =
        let t_index = get_event_length() +1

        let local_event = {
            index = t_index,
            owner = Call.caller,
            name = name',
            location = location',
            cover    = cover',
            price  = price' ,
            description = description',
            paid   = 0,
            up_vote   = 0,
            down_vote = 0,
            total_paid = 0,
            history = []
            }
        put(state{local_events[t_index] = local_event,event_length = t_index })
    
    stateful entrypoint change_event_location(index : int,new_location: string) =
        let local_event = get_local_event(index)
        //require(local_event.owner == Call.caller, "You cannot change event  your don't own ")
        let update_event    =  state.local_events{[index].location = new_location }
        put(state {local_events  =  update_event })

    entrypoint get_event_length() : int =
       state.event_length


    entrypoint get_history_length(event_index:int) : int =
        List.length(state.local_events[event_index].history)

    entrypoint get_local_event(index : int) : local_event =
        switch(Map.lookup(index, state.local_events))
                   None => abort("There was no Local event found with this index registered")
                   Some(value) => value
        
        
    /* this function are used for payment in specific local events such ass wedding or any entertaiment/fund raising
    */

    payable stateful entrypoint pay_for_event(index: int) =
         let local_event = get_local_event(index)

         Chain.spend(local_event.owner,Call.value)
         let amount =local_event.paid + Call.value
         let count =local_event.total_paid + 1
         let update_local_event=state.local_events{[index].paid = amount }
         put(state {local_events = update_local_event})

         let update_count =state.local_events{[index].total_paid = count }
         put(state {local_events = update_count}) 

         let id = get_history_length(index)+1
         let history = {
                    nick_name = String.concat("User_", Int.to_str(id)),
                    txt_id = id,
                    event_address =local_event.owner,
                    txt_ownwer = Call.caller,
                    amount = Call.value
             }
         let update_history = state.local_events{[index].history = history::state.local_events[index].history}
         
        
         put(state {local_events = update_history}) 


    /*
      this functon perform refund of AE coins to the user incase of any issue happening
    */
     
    payable  stateful entrypoint refund(event_id : int,txt_id : int,to : address) = 
           let local_event = get_local_event(event_id)
           let history =local_event.history

           Chain.spend(to, Call.value)
           let amount =local_event.paid - Call.value
           let count =local_event.total_paid - 1
           let update_local_event=state.local_events{[event_id].paid = amount }
           put(state {local_events = update_local_event})
         
          
           let remain_transactions= delete_transaction_by_id(txt_id, history)
           let update_history = state.local_events{[local_event.index].history = remain_transactions}
           put(state {local_events = update_history}) 
   
    stateful entrypoint up_vote(index : int) =
        let local_event = get_local_event(index)
       // require(local_event.owner == Call.caller, "You cannot vote for your own event")
        let up =  local_event.up_vote+1
        let update_event    =  state.local_events{[index].up_vote = up }
        put(state {local_events  =  update_event })
 
    stateful entrypoint down_vote(index : int) =
        let local_event = get_local_event(index)
       // require(local_event.owner == Call.caller, "You cannot vote for your own event")
        let down =  local_event.down_vote+1
        let update_event    =  state.local_events{[index].down_vote = down }
        put(state {local_events  =  update_event })

    /* this function return specific user transaction by id
    */
    function get_transaction_by_id(id : int, transactions : list(users_transaction)) =                                                                                                            
        List.find((t) => t.txt_id == id, transactions)                                                                                                                                     

      
    /* this function update amount for specific user transaction by id
    */                                                                                                                                                                                        
    // function update_amount_by_id(id : int, amount : int, transactions : list(users_transaction)) =                                                                                                
    //  let (my_ts, rest) = List.partition((t) => t.txt_id == id, transactions)                                                                                                            
    //  switch(my_ts)                                                                                                                                                                              
    //   [] => abort("no such id!")                                                                                                                                                               
    //   [my_t] => my_t{amount = amount}::rest                                                                                                                                                    
    //   _ => abort("id not unique!")
    
    
    /* this function filter taransactions that are refunded back to user and return remaining transaction
    */
    function delete_transaction_by_id(id:int,transactions:list(users_transaction)):list(users_transaction) =
        let rest_tranasction = List.filter((t) => t.txt_id != id,transactions)
        rest_tranasction

`;
var contractAddress= "ct_MeU3r6RHaMccgDBUuMiTmsDdc7rrbeM3Jrr4rMNBqzH6snJrc";

var client =null;

var eventsArray = [];
var eventsLength =0;
var userLength=0;

async function renderLocalEvents() {
    var template=$('#template').html();
    Mustache.parse(template);
    var render = Mustache.render(template, {eventsArray});
    $('#events').html(render);
   
}

async function callStatic(func,args){
    const contract = await client.getContractInstance(contractSource, {contractAddress});
   
    const calledGet =await contract.call(func,args,{callStatic : true}).catch(e =>console.error(e))

    const decodedGet = await calledGet.decode().catch(e =>console.error(e));
    console.log(decodedGet)
    return decodedGet;
}

async function contractCall(func, args,value) {
    const contract = await client.getContractInstance(contractSource, {contractAddress});
   
    const calledGet =await contract.call(func,args,{amount : value}).catch(e =>console.error(e))
    console.log(calledGet)
    return calledGet;
  }

window.addEventListener('load',async () =>{
    $('#loader').show();
    client = await Ae.Aepp();

    eventsLength = await callStatic('get_event_length', []);
    console.log(eventsLength);
    for (let i = 1; i <= eventsLength; i++) {
       const evt = await callStatic('get_local_event',[i]);
       userLength =await callStatic('get_history_length',[evt.index]);
         console.log(evt);
       eventsArray.push({
            id       : evt.index,
            owner    : evt.owner,
            name     : evt.name,
            location : evt.location,
            cover    : evt.cover,
            price    : evt.price,
            description     : evt.description,
            paid     : evt.paid,
            up_vote   : evt.up_vote,
            down_vote  : evt.down_vote,
            total_paid     : evt.total_paid,
            history : evt.history,
            tota_user: userLength
        })

        
    }
 renderLocalEvents();
$('#loader').hide();

});

 addNewEvent() 
 payForEvent()
 refund()
 changeLocation()
 upVotEvent()
 downVoteEvent()

function addNewEvent(){
    $(document).on('click','#saveBtn', async function(){
        $('#loader').show();
        const name = $('#name').val();
        const location = $('#location').val();
        const cover = $('#cover').val();
        const price = $('#price').val();
        const description = $('#description').val();
    
        console.log(" name: "+name+" loc: "+location+" cover: "+cover+" price: "+price+" desc: "+description)
    
    
    
    await contractCall('add_event',[name, location,cover,price,description], 0);
         window.location.reload((true));
         renderLocalEvents();
         $('#loader').hide();
    });
  }

  function payForEvent(){
    $('#events').on('click','.payBtn', async function(e){
        $('#loader').show();
        const evt_id = e.target.id;
        const amount = $('input[id='+evt_id+']').val();
        console.log("id="+evt_id+" anount="+amount)
        await contractCall('pay_for_event',[evt_id], amount);
        window.location.reload((true));
        renderLocalEvents();
        $('#loader').hide();
      });
  }

 async function upVotEvent(){
    $('#events').on('click','.upVoteBtn', async function(e){
        $('#loader').show();
        const evt_id = e.target.id;
        await contractCall('up_vote',[evt_id], 0);
        window.location.reload((true));
        renderLocalEvents();
        $('#loader').hide();
      });

  }
 async function downVoteEvent(){
    $('#events').on('click','.downVoteBtn', async function(e){
        $('#loader').show();
        const evt_id = e.target.id;
        await contractCall('down_vote',[evt_id], 0);
        window.location.reload((true));
        renderLocalEvents();
        $('#loader').hide();
      });
  }

 async function changeLocation(){
    $('#events').on('click','.changeLocationBtn', async function(e){
        $('#loader').show();
        const evt_id = e.target.id;
        const new_location = $('input[id='+evt_id+']').val();
        await contractCall('change_event_location',[evt_id,new_location], 0);
        window.location.reload((true));
        renderLocalEvents();
        $('#loader').hide();
      });
  }

  async function refund(){
    $('#events').on('click','.refundBtn', async function(e){
        $('#loader').show();
        const evt_id = e.target.id;
        const tx_id = 1
        const amount = $('input[id='+'p'+evt_id+']').val();
        console.log("id="+evt_id+" anount="+amount)
        await contractCall('refund',[evt_id,tx_id], amount);
        window.location.reload((true));
        renderLocalEvents();
        $('#loader').hide();
      });
  }