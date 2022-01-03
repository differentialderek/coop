# Co-op

This is the code for an on-chain coop, built on Tezos.

Functionality (standard FA2 token):
* Buy outstanding tokens
* Transfer tokens (standard for now but can be restricted)
* Get balance 
* Grant/revoke permissions to others to spend your tokens
* Update metadata 

Functionality (coop): 
* Make a proposal to be voted on (only the admin can propose)
* Vote on a proposal (one token, one vote)
* Discharge funds in the treasury 
* Issue new coop membership tokens 

Housekeeping (mostly coop):
* Update price oracle to calculate USD/XTZ exchange rates
* Redeem tokens if fundraising fails 
* Allow for token transfer (if restricted)