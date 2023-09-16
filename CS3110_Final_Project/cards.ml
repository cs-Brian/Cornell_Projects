type cardType =
  | CommunityChest
  | Chance

type action = 
  | Collect of int 
  | Move of int
  | Keep
  | All of int
  | Taxes of (int * int)

type card = {cardType: cardType; text: string; action: action}

let card1 = {
  cardType= CommunityChest; 
  text= "Advance to Go: Collect $200";
  action= Collect(200);
}
let card2 = {
  cardType= CommunityChest; 
  text= "Inheritance: Collect $100";
  action= Collect(100);
}
let card3 = {
  cardType= CommunityChest; 
  text= "Money Pool: Collect $50 FROM EACH PLAYER";
  action= All(~-50);
}
let card4 = {
  cardType= CommunityChest;
  text= "Pay Day: Collect $25 FOR SERVICES";
  action= Collect(25);
}
let card5 = {
  cardType= CommunityChest;
  text= "You've been caught embezzling money: GO TO JAIL, 
     DO NOT Collect $200 FROM GO";
  action= Move(10);
}
let card6 = {
  cardType= CommunityChest;
  text= "Pay Day: COLLECT $100";
  action= Collect(100);
}
let card7 = {
  cardType = CommunityChest;
  text= "You won second prize in a beauty contest: Collect $100";
  action= Collect(100);
}
let card8 = {
  cardType= CommunityChest;
  text= "Stocks Sold: Collect $45";
  action= Collect(45);
}
let card9 = {
  cardType= CommunityChest;
  text= "Hospital Fees: PAY $100";
  action= Collect(~-100);
}
let card10 = {
  cardType= CommunityChest;
  text= "Income Tax Refund: Collect $20";
  action= Collect(20);
}
let card11 = {
  cardType= CommunityChest;
  text= "Doctor's Fee: PAY $50";
  action= Collect(~-50);
}
let card12 = {
  cardType= CommunityChest;
  text= "Bursar Bill Refund!: Collect $200";
  action= Collect(200);
}
let card13 = {
  cardType= CommunityChest;
  text= "Life Insurance: Collect $100";
  action= Collect(100);
}
let card14 = {
  cardType= CommunityChest;
  text= "School Tax: PAY $150";
  action= Collect(~-150);
}
let card15 = {
  cardType= CommunityChest;
  text= "Taxes: FOR EACH HOUSE PAY $40, FOR EACH HOTEL PAY $115";
  action= Taxes((40, 115));
}

let card16 = {
  cardType= CommunityChest;
  text= "Get out of jail free card";
  action= Keep;
}

let card17 = {
  cardType= Chance; 
  text= "Elected Chairman of the Board: PAY EACH PLAYER $50";
  action= All(50);
}

let card18 = {
  cardType= Chance; 
  text= "This card can be kept or sold: GET OUT OF JAIL FREE";
  action= Keep;
}

let card19 = {
  cardType= Chance; 
  text= "Taxes: PAY POOR TAX OF $15";
  action= Collect(~-15);
}

let card20 = {
  cardType= Chance; 
  text= "Advance to Go: Collect $200";
  action= Collect(200);
}

let card21 = {
  cardType= Chance; 
  text= "You dropped something: GO BACK 3 SPACES";
  action= Move(~-3);
}

let card22 = {
  cardType= Chance; 
  text= "Go to the nearest railroad: IF UNOWNED YOU MAY BUY IT FROM THE BANK. 
     ELSE, PAY THE OWNER TWICE";
  action= Move(5);
}

let card23 = {
  cardType= Chance; 
  text= "Go to the nearest railroad: IF UNOWNED YOU MAY BUY IT FROM THE BANK. 
     ELSE, PAY THE OWNER TWICE";
  action= Move(5);
}

let card24 = {
  cardType= Chance; 
  text= "You've been caught embezzling money: GO TO JAIL, 
     DO NOT Collect $200 FROM GO";
  action= Move(10);
}

let card25 = {
  cardType= Chance; 
  text= "Your building and loan matures: Collect $150";
  action= Collect(150);
}

let card26 = {
  cardType= Chance; 
  text= "Dividends: Collect $50";
  action= Collect(50);
}

let card27 = {
  cardType= Chance; 
  text= "A philanthropist was feeling generous! Collect $200";
  action= Collect(200);
}

let card28 = {
  cardType= Chance; 
  text= "Advance to St.Charles Place: IF YOU PASS GO, Collect $200";
  action= Collect(200);
}

let card29 = {
  cardType= Chance; 
  text= "Advance to the nearest utility: 
     IF UNOWNED YOU MAY BUY IT FROM THE BANK. 
     ELSE, PAY THE OWNER 10X ROLL AMOUNT";
  action= Move(20);
}

let card30 = {
  cardType= Chance; 
  text= "Take a stoll on the magnificent mile: ADVANCE TO BOARDWALK";
  action= Move(39);
}

let card31 = {
  cardType= Chance; 
  text= "Catch the bus: ADVANCE TO ILLINOIS AVE.";
  action= Move(24);
}

let card32 = {
  cardType= Chance; 
  text= "Repair and upgrade your houses and hotels: 
     FOR EACH HOUSE PAY $25,
     FOR EACH HOTEL PAY $100";
  action= Taxes((25, 100));
}

let community_chest_deck = [
  card16; card2; card3; card4; card5; card6; card7; card8; 
  card9; card10; card11; card12; card13; card14; card15; card1
]

let chance_deck = [
  card18; card17; card19; card20; card21; card22; card23; card24; 
  card25; card26; card27; card28; card29; card30; card31; card32
]