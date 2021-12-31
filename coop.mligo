// TODO: some upgradeability mechanism 
// TODO: views for balances 
// TODO: yea vs nay spelling?

(* =============================================================================
 * Storage 
 * ============================================================================= *)

type proposal_id = nat 
type proposal = {
    proposal : bytes ; 
    yea : nat ; 
    nay : nat ; 
    abstain : nat ; 
    deadline : timestamp ; 
    result : bool ; // TODO : what about a tie?
}
type vote = 
| Yea of unit 
| Nay of unit 
| Abstain of unit 

type token_id = nat 
type operators = [@layout:comb] {
    token_owner : address ; 
    token_operator : address ; 
    token_id : token_id ;
}
type token_metadata = [@layout:comb]{
    token_id : token_id ; 
    token_info : (string, bytes) map ;
}

// storage 
type storage = {
    // the contract admin/governance body
    admin : address ; 
    oracle : address ; // the Kolibri oracle for the USD/XTZ exchange rate 

    // outstanding tokens 
    outstanding_tokens : nat ;

    // coop membership token ownership ledger
    ledger : (address * nat, nat) big_map ; 

    // proposals and votes for the coop
    proposals : (proposal_id, proposal) big_map ;
    votes : (address * proposal_id, vote) big_map ; 

    // an operator can trade tokens on behalf of the fa2_owner
    // if the key (owner, operator, token_id) returns some k : nat, this denotes that the operator has (one-time?) permissions to operate k tokens
    // if there is no entry, the operator has no permissions
    // such permissions need to granted, e.g. for the burn entrypoint in the carbon contract
    operators : (operators, nat) big_map;
    // token metadata for each token type supported by this contract
    token_metadata : (token_id, token_metadata) big_map;
    // contract metadata 
    metadata : (string, bytes) big_map;
}

(* =============================================================================
 * Entrypoints
 * ============================================================================= *)

type token_metadata = [@layout:comb]{
    token_id : nat ; 
    token_info : (string, bytes) map ;
}
type contract_metadata = (string, bytes) big_map


type buy = {
    min_amt : nat ; // the min amt of tokens wishing to be bought 
} 
type propose = {
    proposal_id : proposal_id ; 
    proposal : proposal ; 
}
type cast_vote = {
    proposal_id : nat ; 
    vote : vote ; 
}    
type discharge_treasury = {
    amt : nat ; // amt to discharge 
    _to : address ; // sends funds to this address 
}

type transfer_to = [@layout:comb]{ to_ : address ; token_id : nat ; amount : nat ; }
type transfer = 
    [@layout:comb]
    { from_ : address; 
      txs : transfer_to list; }

type requests = [@layout:comb]{ owner : address ; token_id : nat ; }
type request = [@layout:comb]{ owner : address ; token_id : nat ; }
type callback_data = [@layout:comb]{ request : request ; balance : nat ; }
type balance_of = [@layout:comb]{
    requests : requests list ; 
    callback : callback_data list contract ;
}

type operator_data = [@layout:comb]{ owner : address ; operator : address ; token_id : nat ; qty : nat ; }
type update_operator = 
    | Add_operator of operator_data
    | Remove_operator of operator_data
type update_operators = update_operator list

type get_metadata = {
    token_ids : nat list ;
    callback : token_metadata list contract ;
}



type entrypoint = 
// fa2 functionality 
| Buy of buy // buy outstanding tokens (use Kolibri oracle) 
| Transfer of transfer list // transfer (but restricted only to owner) ... sell instead?
| Balance_of of balance_of // query an address's balance
| Update_operators of update_operators // change operators for some address
| Get_metadata of get_metadata // query the metadata of a given token
| Update_contract_metadata of contract_metadata
// coop functionality 
| Propose of propose // admin/governance gives the coop something to vote on 
| Cast_vote of cast_vote // cast a vote
| Discharge_treasury of discharge_treasury // send an amt from the contract's balance to an address
| Issue_tokens of nat // issue more coop membership tokens

// result
type result = operation list * storage 


(* =============================================================================
 * Errors
 * ============================================================================= *)

let error_FA2_TOKEN_UNDEFINED = 0n // One of the specified token_ids is not defined within the FA2 contract
let error_FA2_INSUFFICIENT_BALANCE = 1n // A token owner does not have sufficient balance to transfer tokens from owner's account
let error_FA2_TX_DENIED = 2n // A transfer failed because of fa2_operatortransfer_policy == No_transfer
let error_FA2_NOT_OWNER = 3n // A transfer failed because fa2_operatortransfer_policy == fa2_ownertransfer and it is invoked not by the token owner
let error_FA2_NOT_OPERATOR = 4n // A transfer failed because fa2_operatortransfer_policy == fa2_owneror_fa2_operatortransfer and it is invoked neither by the token owner nor a permitted operator
let error_FA2_OPERATORS_UNSUPPORTED = 5n // update_operators entrypoint is invoked and fa2_operatortransfer_policy is No_transfer or fa2_ownertransfer
let error_FA2_RECEIVER_HOOK_FAILED = 6n // The receiver hook failed. This error MUST be raised by the hook implementation
let error_FA2_SENDER_HOOK_FAILED = 7n // The sender failed. This error MUST be raised by the hook implementation
let error_FA2_RECEIVER_HOOK_UNDEFINED = 8n // Receiver hook is required by the permission behavior, but is not implemented by a receiver contract
let error_FA2_SENDER_HOOK_UNDEFINED = 9n // Sender hook is required by the permission behavior, but is not implemented by a sender contract
let error_PERMISSIONS_DENIED = 10n // General catch-all for operator-related permission errors
let error_ID_ALREADY_IN_USE = 11n // A token ID can only be used once, error if a user wants to add a token ID that's already there
let error_COLLISION = 12n
let error_NO_PROPOSAL_FOUND = 13n 
let error_MUST_HAVE_A_NONZERO_BALANCE_TO_VOTE = 14n
let error_VOTE_ALREADY_CAST = 15n
let error_DEADLINE_IS_PASSED = 16n
let error_INVALID_ADDRESS = 17n



(* =============================================================================
 * Aux Functions
 * ============================================================================= *)

// an auxiliary function for querying an address's balance
type fa2_token_id = nat 
type fa2_amt = nat
type fa2_owner = address
type fa2_operator = address

let rec owner_and_id_to_balance (param : (callback_data list) * (requests list) * ((fa2_owner * fa2_token_id , fa2_amt) big_map)) : callback_data list =
    let (accumulator, request_list, ledger) = param in
    match request_list with
    | [] -> accumulator 
    | h :: t -> 
        let owner = h.owner in 
        let token_id = h.token_id in
        let qty =
            match Big_map.find_opt (owner, token_id) ledger with 
            | None -> 0n
            | Some owner_balance -> owner_balance in
        let request = { owner = owner ; token_id = token_id ; } in
        let accumulator = { request = request ; balance = qty ; } :: accumulator in
        owner_and_id_to_balance (accumulator, t, ledger) 


(* =============================================================================
 * Entrypoint Functions
 * ============================================================================= *)

// buy outstanding tokens 
let buy (_param : buy) (storage : storage) : result = ([] : operation list), storage 
    // use the Kolibri oracle to calculate tokens being bought at 1 token = 1 USD
    // get amt in tez, convert to tokens, make sure it's >= the min tokens bought threshold, and execute the txn


// transfer coop membership tokens 
let rec transfer_txn (param , storage : transfer * storage) : storage = 
    match param.txs with
    | [] -> storage
    | hd :: tl ->
        let (from, to, token_id, qty) = (param.from_, hd.to_, hd.token_id, hd.amount) in 
        // check permissions
        let operator = Tezos.sender in 
        let owner = from in 
        let operator_permissions = 
            match Big_map.find_opt { token_owner = owner; token_operator = operator; token_id = token_id; } storage.operators with 
            | None -> 0n
            | Some allowed_qty -> allowed_qty in 
        if ((Tezos.sender <> from) && (operator_permissions < qty)) then (failwith error_FA2_NOT_OPERATOR : storage) else 
        // update operator permissions to reflect this transfer
        let operators = 
            if Tezos.sender <> from // thus this is an operator
            then Big_map.update { token_owner = owner; token_operator = operator; token_id = token_id; } (Some (abs (operator_permissions - qty))) storage.operators
            else storage.operators in
        // check balance
        let sender_token_balance =
            match Big_map.find_opt (from, token_id) storage.ledger with
            | None -> 0n
            | Some token_balance -> token_balance in
        let recipient_balance = 
            match Big_map.find_opt (to, token_id) storage.ledger with
            | None -> 0n
            | Some recipient_token_balance -> recipient_token_balance in
        if (sender_token_balance < qty) then (failwith error_FA2_INSUFFICIENT_BALANCE : storage) else
        // update the ledger
        let ledger = 
            Big_map.update
            (to, token_id)
            (Some (recipient_balance + qty))
                (Big_map.update 
                 (from, token_id) 
                 (Some (abs (sender_token_balance - qty))) 
                 storage.ledger) in 
        let storage = {storage with ledger = ledger ; operators = operators ; } in
        let param = { from_ = from ; txs = tl ; } in 
        transfer_txn (param, storage)

let rec transfer (param, storage : transfer list * storage) : result = 
    match param with 
    | [] -> (([] : operation list), storage)
    | hd :: tl -> 
        let storage = transfer_txn (hd, storage) in 
        transfer (tl, storage)


// the entrypoint to query balance 
// input balance_of is a tuple:
//   * the first entry is a list of the form (owner, token_id) list which queries the balance of owner in the given token id
//   * the second entry is a contract that can receive the list of balances. This list is of the form 
//     (owner, token_id, amount) list = (address * nat * nat) list
//     An example of such a contract is in tests/test-fa2.mligo 
let balance_of (param : balance_of) (storage : storage) : result = 
    let (request_list, callback) = (param.requests, param.callback) in 
    let accumulator = ([] : callback_data list) in
    let ack_list = owner_and_id_to_balance (accumulator, request_list, storage.ledger) in
    let t = Tezos.transaction ack_list 0mutez callback in
    ([t], storage)


// The entrypoint where fa2_owner adds or removes fa2_operator from storage.operators
// * The input is a triple: (owner, operator, id) : address * address * nat
//   This triple is tagged either as Add_operator or Remove_operator
// * Only the token owner can add or remove operators
// * An operator can perform transactions on behalf of the owner
let update_operator (param : update_operator) (storage : storage) : storage = 
    match param with
    | Add_operator o ->
        let (owner, operator, token_id, qty) = (o.owner, o.operator, o.token_id, o.qty) in 
        // check permissions        
        if (Tezos.source <> owner) then (failwith error_PERMISSIONS_DENIED : storage) else
        if operator = owner then (failwith error_COLLISION : storage) else // an owner can't be their own operator 
        // update storage
        let new_qty = 
            let old_qty = 
             match Big_map.find_opt { token_owner = owner; token_operator = operator; token_id = token_id; } storage.operators with 
             | None -> 0n 
             | Some q -> q in 
            old_qty + qty in 
        let storage = {storage with 
            operators = Big_map.update { token_owner = owner; token_operator = operator; token_id = token_id; } (Some new_qty) storage.operators ; } in 
        storage
    | Remove_operator o ->
        let (owner, operator, token_id) = (o.owner, o.operator, o.token_id) in 
        // check permissions
        if (Tezos.sender <> owner) then (failwith error_PERMISSIONS_DENIED : storage) else
        // update storage
        let storage = {storage with 
            operators = Big_map.update { token_owner = owner; token_operator = operator; token_id = token_id; } (None : nat option) storage.operators ; } in 
        storage

let rec update_operators (param, storage : update_operators * storage) : result = 
    match param with
    | [] -> (([] : operation list), storage)
    | hd :: tl -> 
        let storage = update_operator hd storage in 
        update_operators (tl, storage)


// The entrypoint to query token metadata
// The input is a tuple: (query_list, callback_contract)
//   * The query list is of token ids and has type `nat list`
//   * The callback contract must have type ((fa2_token_id * token_metadata) list contract)
let get_metadata (param : get_metadata) (storage : storage) : result = 
    let query_list = param.token_ids in 
    let callback = param.callback in 
    let metadata_list = 
        List.map 
        (fun (token_id : nat) : token_metadata -> 
            match Big_map.find_opt token_id storage.token_metadata with 
            | None -> (failwith error_FA2_TOKEN_UNDEFINED : token_metadata) 
            | Some m -> {token_id = token_id ; token_info = m.token_info ; })
        query_list in 
    let op_metadata = Tezos.transaction metadata_list 0tez callback in 
    ([op_metadata] , storage)


// this entrypoint allows a project owner to update the metadata for their project
let update_contract_metadata (param : contract_metadata) (storage : storage) : result = 
    if Tezos.sender <> storage.admin then (failwith error_PERMISSIONS_DENIED : result) else
    ([] : operation list),
    { storage with metadata = param }


(* ==========
 * Coop Mechanism Entrypoint Functions 
 * ========== *)

// propose something for the coop to vote on 
let propose (param : propose) (storage : storage) : result = 
    let (proposal_id, proposal) = (param.proposal_id, param.proposal) in 
    // check permissions
    if Tezos.sender <> storage.admin then (failwith error_PERMISSIONS_DENIED : result) else 
    // check there is no existing proposal with the same proposal id
    if Big_map.mem proposal_id storage.proposals then (failwith error_COLLISION : result) else 
    // add the proposal
    ([] : operation list),
    { storage with 
        proposals = Big_map.update proposal_id (Some proposal) storage.proposals ; }


// vote on a proposal 
let cast_vote (param : cast_vote) (storage : storage) : result = 
    let (proposal_id, vote) = (param.proposal_id, param.vote) in 
    // check that Tezos.sender has a balance and capture that balance
    let vote_qty = // one token, one vote 
        match Big_map.find_opt (Tezos.sender, 0n) storage.ledger with 
        | None -> (failwith error_MUST_HAVE_A_NONZERO_BALANCE_TO_VOTE : nat)
        | Some bal -> 
            if bal > 0n then bal else (failwith error_MUST_HAVE_A_NONZERO_BALANCE_TO_VOTE : nat) in 
    // update storage.proposals; check the deadline for voting is still in the future
    let proposals = 
        let proposal = 
            match Big_map.find_opt proposal_id storage.proposals with
            | Some p -> (
                // check the deadline for voting is still in the future
                if p.deadline < Tezos.now then (failwith error_DEADLINE_IS_PASSED : proposal) else
                // update the vote tally 
                match vote with 
                | Yea _ -> {p with yea = p.yea + vote_qty}
                | Nay _ -> {p with nay = p.nay + vote_qty}
                | Abstain _ -> {p with abstain = p.abstain + vote_qty})
            | None -> (failwith error_NO_PROPOSAL_FOUND : proposal) in 
        Big_map.update proposal_id (Some proposal) storage.proposals in 
    // update storage.votes 
    let votes = 
        match Big_map.get_and_update (Tezos.sender, proposal_id) (Some vote) storage.votes with 
        | (None,   v) -> v // the voter hasn't already cast a vote 
        | (Some _, v) -> (failwith error_VOTE_ALREADY_CAST : (address * proposal_id, vote) big_map) in 
    // finish 
    ([] : operation list),
    { storage with 
        proposals = proposals ; 
        votes = votes ; }


// discharge funds from the treasury 
let discharge_treasury (param : discharge_treasury) (storage : storage) : result = 
    let (amt, _to) = (param.amt, param._to) in 
    // check permissions 
    if Tezos.sender <> storage.admin then (failwith error_PERMISSIONS_DENIED : result) else 
    // create the operation to discharge funds 
    let op_dischargeFunds = 
        let recipient = 
            match (Tezos.get_contract_opt _to : unit contract option) with 
            | None -> (failwith error_INVALID_ADDRESS : unit contract)
            | Some c -> c in 
        Tezos.transaction () (amt * 1mutez) recipient in 
    // finish
    [ op_dischargeFunds ; ], 
    storage 


// issue tokens entrypoint 
let issue_tokens (param : nat) (storage : storage) : result = 
    // check permissions 
    if Tezos.sender <> storage.admin then (failwith error_PERMISSIONS_DENIED : result) else 
    // issue tokens 
    ([] : operation list),
    { storage with outstanding_tokens = storage.outstanding_tokens + param }


(* =============================================================================
 * Main Function
 * ============================================================================= *)

let rec main (entrypoint, storage : entrypoint * storage) : result = 
    match entrypoint with 
    // the FA2 mechanisms 
    | Buy param -> 
        buy param storage 
    | Transfer param -> 
        transfer (param, storage)
    | Balance_of param -> 
        balance_of param storage
    | Update_operators param ->
        update_operators (param, storage)
    | Get_metadata param ->
        get_metadata param storage
    | Update_contract_metadata param ->
        update_contract_metadata param storage
    // coop governance mechanisms
    | Propose param -> 
        propose param storage 
    | Cast_vote param -> 
        cast_vote param storage 
    | Discharge_treasury param -> 
        discharge_treasury param storage 
    | Issue_tokens param -> 
        issue_tokens param storage 