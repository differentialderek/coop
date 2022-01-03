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

type voter_data = [@layout:comb]{ voter : address ; proposal_id : nat ;}
type vote = 
| Yea of unit 
| Nay of unit 
| Abstain of unit 

type token_id = nat
type qty = nat

type owner = [@layout:comb] { token_owner : address ; token_id : nat ; }
type operator = [@layout:comb] {
    token_owner : address ; 
    token_operator : address ; 
    token_id : nat ;
}

type token_metadata = [@layout:comb]{ token_id : nat ; token_info : (string, bytes) map ; }
type contract_metadata = (string, bytes) big_map

// storage 
type storage = {
    // the contract admin/governance body
    admin : address ; 
    tz_usd_oracle : address ; // the Harbinger oracle for the USD/XTZ exchange rate 

    // outstanding tokens 
    outstanding_tokens : nat ; // with six digits of precision, i.e. 1 token = 1_000_000n
    total_token_supply : nat ; // with six digits of precision, i.e. 1 token = 1_000_000n

    // coop membership token ownership ledger
    ledger : (owner , qty) big_map ; 

    // proposals and votes for the coop
    proposals : (proposal_id, proposal) big_map ;
    votes : (voter_data, vote) big_map ; 

    // an operator can trade tokens on behalf of the fa2_owner
    // if the key (owner, operator, token_id) returns some k : nat, this denotes that the operator has (one-time?) permissions to operate k tokens
    // if there is no entry, the operator has no permissions
    // such permissions need to granted, e.g. for the burn entrypoint in the carbon contract
    operators : (operator, nat) big_map;
    // token metadata for each token type supported by this contract
    token_metadata : (token_id, token_metadata) big_map;
    // contract metadata 
    metadata : (string, bytes) big_map;

    // fundraising details 
    can_redeem : bool ;
    can_transfer : bool ; // for compliance
}

(* =============================================================================
 * Entrypoints
 * ============================================================================= *)

// entrypoint types for the fa2 contract
type transfer_to = [@layout:comb]{ to_ : address ; token_id : nat ; amount : nat ; }
type transfer = [@layout:comb] { from_ : address; txs : transfer_to list; }

type request = [@layout:comb]{ token_owner : address ; token_id : nat ; }
type callback_data = [@layout:comb]{ request : request ; balance : nat ; }
type balance_of = [@layout:comb]{ requests : request list ; callback : callback_data list contract ; }

type operator_data = [@layout:comb]{ owner : address ; operator : address ; token_id : nat ; qty : nat ; }
type update_operator = 
    | Add_operator of operator_data
    | Remove_operator of operator_data
type update_operators = update_operator list

type get_metadata = {
    token_ids : nat list ;
    callback : token_metadata list contract ;
}

// entrypoint types for the coop mechanism
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
// housekeeping 
| Update_oracle of address // entrypoint to update the tz/usd oracle
| Redeem_tokens of unit // if fundraising fails, buyers can redeem their tokens 
| Allow_transfer of unit // 

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
let error_INSUFFICIENT_TOKENS_BOUGHT = 18n
let error_ORACLE_FAILED = 19n



(* =============================================================================
 * Aux Functions
 * ============================================================================= *)

let min_nat (n : nat) (m : nat) : nat = 
    if n < m then n else m 

(* =============================================================================
 * Entrypoint Functions
 * ============================================================================= *)

// buy outstanding tokens 
let buy (param : buy) (storage : storage) : result = 
    let (tez_amt, min_amt, buyer) = (Tezos.amount, param.min_amt, Tezos.source) in 
    // use the Kolibri oracle to calculate tokens being bought at 1 token = 1 USD
    let tokens_bought : nat = 
        // get the price of tz in usd 
        let tz_usd = 
            match (Tezos.call_view "tz_usd" () storage.tz_usd_oracle : nat option) with 
            | None -> (failwith error_ORACLE_FAILED : nat)
            | Some r -> r in 
        // calculate the tokens bought 
        min_nat (tez_amt / 1mutez * tz_usd) storage.outstanding_tokens in 
    // check min conditions met 
    if tokens_bought < min_amt then (failwith error_INSUFFICIENT_TOKENS_BOUGHT : result) else 
    // update the ledger with the buyer's new balance 
    let ledger = 
        let new_bal = 
            match Big_map.find_opt {token_owner = buyer; token_id = 0n ;} storage.ledger with 
            | None -> tokens_bought 
            | Some b -> b + tokens_bought in 
        Big_map.update {token_owner = buyer; token_id = 0n ;} (Some new_bal) storage.ledger in 
    // update outstanding tokens 
    let outstanding_tokens = abs (storage.outstanding_tokens - tokens_bought) in 
    // finish 
    ([] : operation list),
    { storage with 
        ledger = ledger ; 
        outstanding_tokens = outstanding_tokens ; }


// The transfer entrypoint function
// The transfer function creates a list of transfer operations recursively
let rec execute_transfer (param , storage : transfer * storage) : storage = 
    match param.txs with
    | [] -> storage
    | hd :: tl ->
        let (from, to, token_id, qty, operator) = (param.from_, hd.to_, hd.token_id, hd.amount, Tezos.sender) in 
        let owner = from in 
        // update operator permissions to reflect this transfer
        let operators = 
            if Tezos.sender <> from // thus this is an operator
            then 
                let allowed_qty = 
                    match Big_map.find_opt {token_owner = owner; token_operator = operator; token_id = token_id ;} storage.operators with 
                    | None -> 0n | Some q -> q in 
                // failw if operator does not have permissions 
                if allowed_qty < qty then (failwith error_FA2_NOT_OPERATOR : (operator, nat) big_map) else 
                Big_map.update {token_owner = owner; token_operator = operator; token_id = token_id ;} (Some (abs (allowed_qty - qty))) storage.operators
            else storage.operators in
        // update the ledger
        let ledger = 
            // check balances 
            let sender_token_balance =
                match Big_map.find_opt {token_owner = from; token_id = token_id ;} storage.ledger with | None -> 0n | Some b -> b in
            let recipient_balance = 
                match Big_map.find_opt {token_owner = to; token_id = token_id ;} storage.ledger with | None -> 0n | Some b -> b in
            // ensure sufficient funds 
            if (sender_token_balance < qty) then (failwith error_FA2_INSUFFICIENT_BALANCE : (owner, qty) big_map) else
            // update the ledger 
            Big_map.update
            { token_owner = to ; token_id = token_id ; }
            (Some (recipient_balance + qty))
                (Big_map.update 
                 {token_owner = from; token_id = token_id ;}
                 (Some (abs (sender_token_balance - qty))) 
                 storage.ledger) in 
        // recurse with the same from_ address
        execute_transfer (
            { from_ = from ; txs = tl ; }, 
            { storage with ledger = ledger ; operators = operators ; }
        )

let rec transfer (param, storage : transfer list * storage) : result = 
    if storage.can_transfer = false then (failwith error_PERMISSIONS_DENIED : result) else
    match param with 
    | [] -> (([] : operation list), storage)
    | hd :: tl -> 
        let storage = execute_transfer (hd, storage) in 
        transfer (tl, storage)


// the entrypoint to query balance 
let balance_of (param : balance_of) (storage : storage) : result = 
    let (request_list, callback) = (param.requests, param.callback) in 
    let op_balanceOf = 
        Tezos.transaction 
        (
            List.map 
            (
                fun (r : request) ->  
                { request = r ; 
                  balance = 
                    match Big_map.find_opt r storage.ledger with | None -> 0n | Some b -> b ; } 
            )
            request_list 
        )
        0mutez 
        callback in
    ([op_balanceOf], storage)


// The entrypoint where fa2_owner adds or removes fa2_operator from storage.operators
let update_operator (storage, param : storage * update_operator) : storage = 
    match param with
    | Add_operator o ->
        let (owner, operator, token_id, qty) = (o.owner, o.operator, o.token_id, o.qty) in 
        // check permissions        
        if (Tezos.source <> owner) then (failwith error_PERMISSIONS_DENIED : storage) else
        if operator = owner then (failwith error_COLLISION : storage) else // an owner can't be their own operator 
        // update storage
        {storage with operators = 
            let new_qty = 
                let old_qty = 
                    match Big_map.find_opt {token_owner = owner; token_operator = operator; token_id = token_id ;} storage.operators with 
                    | None -> 0n 
                    | Some q -> q in 
                old_qty + qty in 
            Big_map.update {token_owner = owner; token_operator = operator; token_id = token_id ;} (Some new_qty) storage.operators ; }
    | Remove_operator o ->
        let (owner, operator, token_id) = (o.owner, o.operator, o.token_id) in 
        // check permissions
        if (Tezos.source <> owner) then (failwith error_PERMISSIONS_DENIED : storage) else
        // update storage
        {storage with 
            operators = Big_map.update {token_owner = owner; token_operator = operator; token_id = token_id ;} (None : nat option) storage.operators ; }
        
let rec update_operators (param, storage : update_operators * storage) : result = 
    ([] : operation list),
    List.fold update_operator param storage 


// The entrypoint to query token metadata
let get_metadata (param : get_metadata) (storage : storage) : result = 
    let (query, callback) = (param.token_ids, param.callback) in 
    let op_metadata = 
        Tezos.transaction
        (
            List.map 
            (fun (token_id : nat) : token_metadata -> 
                match Big_map.find_opt token_id storage.token_metadata with 
                | None -> (failwith error_FA2_TOKEN_UNDEFINED : token_metadata) 
                | Some m -> {token_id = token_id ; token_info = m.token_info ; })
            query
        )
        0tez 
        callback in 
    ([op_metadata] , storage)


// this entrypoint allows the admin to update the contract metadata
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
        match Big_map.find_opt {token_owner = Tezos.sender; token_id = 0n ;} storage.ledger with 
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
        match Big_map.get_and_update {voter = Tezos.sender; proposal_id = proposal_id;} (Some vote) storage.votes with 
        | (None,   v) -> v // the voter hasn't already cast a vote 
        | (Some _, v) -> (failwith error_VOTE_ALREADY_CAST : (voter_data, vote) big_map) in 
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
    { storage with 
        outstanding_tokens = storage.outstanding_tokens + param ;
        total_token_supply = storage.total_token_supply + param ; }

(* ==========
 * Housekeeping Functions 
 * ========== *)

let update_oracle (param : address) (storage : storage) : result = 
    // check permissions
    if Tezos.sender <> storage.admin then (failwith error_PERMISSIONS_DENIED : result) else 
    ([] : operation list),
    { storage with 
      tz_usd_oracle = param ; }

let redeem_tokens (storage : storage) : result = 
    if storage.can_redeem = false then (failwith error_PERMISSIONS_DENIED : result) else 
    // update ledger and get their token balance 
    let (token_balance, ledger) = 
        match Big_map.get_and_update { token_owner = Tezos.source ; token_id = 0n ;} (None : nat option) storage.ledger with
        | (None, l) -> (0n, l)
        | (Some b, l) -> (b, l) in 
    // make the operation that returns XTZ
    let op_redeem_tokens = 
        let tz_to_receive = 
            token_balance * Tezos.balance / storage.total_token_supply in 
        let recipient_contract = 
            match (Tezos.get_contract_opt Tezos.source : unit contract option) with 
            | None -> (failwith error_INVALID_ADDRESS : unit contract)
            | Some c -> c in 
        Tezos.transaction () tz_to_receive recipient_contract in 
    // finish 
    [ op_redeem_tokens ;], 
    {storage with ledger = ledger ;}

let allow_transfer (storage : storage) : result = 
    if Tezos.source <> storage.admin then (failwith error_PERMISSIONS_DENIED : result) else 
    ([] : operation list),
    { storage with can_transfer = true ; }

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
    // housekeeping 
    | Update_oracle param ->
        update_oracle param storage
    | Redeem_tokens _ ->
        redeem_tokens storage
    | Allow_transfer _ ->
        allow_transfer storage