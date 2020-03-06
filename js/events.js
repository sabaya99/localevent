var contractSource = `
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
        index : int,
        txt_ownwer : address,
        amount :int
        }
    record state = {
        local_events : map(int, local_event), 
        users_transactions : map(int ,users_transaction),
        event_length : int,
        history_length : int
        }


    stateful entrypoint init() = { 
        event_length = 0,
        history_length = 0,
        local_events = {},
        users_transactions = {}
        }
    
    
    public stateful entrypoint add_event(name': string, location': string,cover' : string,price' : int,description' :string) =
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
    
    public stateful entrypoint change_event_location(index : int,new_location: string) =
        let local_event = get_local_event(index)
        //require(local_event.owner == Call.caller, "You cannot change event  your don't own ")
        let update_event    =  state.local_events{[index].location = new_location }
        put(state {local_events  =  update_event })

    public entrypoint get_event_length() : int =
       state.event_length


    public entrypoint get_history_length() : int =
         state.history_length

    public entrypoint get_local_event(index : int) : local_event =
        switch(Map.lookup(index, state.local_events))
                   None => abort("There was no Local event found with this index registered")
                   Some(value) => value
        

    payable stateful entrypoint pay_for_event(index: int) =
         let local_event = get_local_event(index)
         Chain.spend(local_event.owner,Call.value)
         let amount =local_event.paid + Call.value
         let count =local_event.total_paid + 1
         let update_local_event=state.local_events{[index].paid = amount }
         let update_count =state.local_events{[index].total_paid = count }
         let history = {
                    index = get_event_length() + 1,
                    txt_ownwer = Call.caller,
                    amount = Call.value
             }
         let update_history = state.local_events{[index].history = history::state.local_events[index].history}
         put(state {local_events = update_local_event})
         put(state {local_events = update_count}) 
         put(state {local_events = update_history}) 

    payable  stateful entrypoint refund(event_id :int,txt_id :int) = 
           let local_event = get_local_event(event_id)
           let history = local_event.history
           history


          
    public stateful entrypoint up_vote(index : int) =
        let local_event = get_local_event(index)
       // require(local_event.owner == Call.caller, "You cannot vote for your own event")
        let up =  local_event.up_vote+1
        let update_event    =  state.local_events{[index].up_vote = up }
        put(state {local_events  =  update_event })
 
    public stateful entrypoint down_vote(index : int) =
        let local_event = get_local_event(index)
       // require(local_event.owner == Call.caller, "You cannot vote for your own event")
        let down =  local_event.down_vote+1
        let update_event    =  state.local_events{[index].down_vote = down }
        put(state {local_events  =  update_event })
`;
var contractAddress= "ct_yQbP8rgZ3Cf6Zt5r8WUsXC8gdtaD2UNWyeyYNZCR7kHPeCjv";

var client =null;

var eventsArray = [];
var eventsLength =0;

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
    
    return decodedGet;
}

async function contractCall(func, args,value) {
    const contract = await client.getContractInstance(contractSource, {contractAddress});
   
    const calledGet =await contract.call(func,args,{amount : value}).catch(e =>console.error(e))

    return calledGet;
  }

window.addEventListener('load',async () =>{
    $('#loader').show();
    client = await Ae.Aepp();

    eventsLength = await callStatic('get_event_length', []);
    console.log(eventsLength);
    for (let i = 1; i <= eventsLength; i++) {
       const evt = await callStatic('get_local_event',[i]);
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
            total_paid     : evt.total_paid 
        })

        
    }
 renderLocalEvents();
$('#loader').hide();
});

$(document).on('click','#saveBtn', async function(){
    $('#loader').show();
    const name = $('#name').val();
    const location = $('#location').val();
    const cover = $('#cover').val();
    const price = $('#price').val();
    const description = $('#description').val();



await contractCall('add_event',[name, location,cover,price,description], 0);
     renderLocalEvents();
     $('#loader').hide();
});


$('#events').on('click','.payBtn', async function(e){
    $('#loader').show();
    const evt_id = e.target.id;
    const amount = $('input[id='+evt_id+']').val();
    await contractCall('pay_for_event',[evt_id], amount);
    location.reload((true));
    renderLocalEvents();
    $('#loader').hide();
  });
  
  
  $('#events').on('click','.upVote', async function(e){
    $('#loader').show();
    const evt_id = e.target.id;
    await contractCall('up_vote',[evt_id], 0);
    location.reload((true));
    renderLocalEvents();
    $('#loader').hide();
  });
  
  $('#events').on('click','.downVote', async function(e){
    $('#loader').show();
    const evt_id = e.target.id;
    await contractCall('down_vote',[evt_id], 0);
    location.reload((true));
    renderLocalEvents();
    $('#loader').hide();
  });

  $('#events').on('click','.changeLocation', async function(e){
    $('#loader').show();
    const evt_id = e.target.id;
    const new_location = $('input[id='+evt_id+']').val();
    await contractCall('change_event_location',[evt_id,new_location], 0);
    location.reload((true));
    renderLocalEvents();
    $('#loader').hide();
  });

