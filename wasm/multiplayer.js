const server = "wss://tesseract.nfshost.com/connect/"
let connected = false

const N_JOIN = 1
const N_WELCOME = 2
const N_CATCHUP = 3
const N_MOVE = 4
const N_UNDO = 5
const N_SUBMIT = 6
const N_RESET = 7
const N_CATCHUPDONE = 8

let ws
let resetVotes = 0

let welcomeState = 0
// 0 means waiting for messages
// 1 means catching up
// 2 means has sent its own 'myType'
// 4 means has sent its own 'myType' twice
// ...124 means has sent its own 'myType' 62 times or more
// 3 = 2|1
// 126 means finished
let welcomeTimeout
function setWelcomeTimeout(){
  clearTimeout(welcomeTimeout)
  welcomeTimeout = setTimeout(()=>{
    if(myType==3){
      myType = Math.random()>0.5?1:2;
    }
    welcomeState=126
    setTurn(col)
  }, 3000)
}
function hasPermission(){
  return (welcomeState==126 && col+1==myType)
}

let myType = 3 // bitmask of colors not held by other players (1=white,2=black)
// 0 means observer
let history = []

//let numPlayers
//let movesSinceLast = -1
//let globalTag = params["multiplayer"]

let lastCatchup = 0

function connect(tag){
  if(connected) ws.close()
  connected=false
  ws = new WebSocket(server+tag)
  ws.onopen = (x => {
    if(x.target===ws){
      console.log("connection started")
      inGame.style.display=""
      noConn.style.display="none"
      gameTag.innerText=tag
      connected = true
      oldReset()
      history = []
      myType = 3
      let c = params["col"]
      if(c)c=c.toLowerCase()
      if(c=="white") myType = 1
      if(c=="black") myType = 2
      ws.send(new Uint8Array([N_JOIN,myType]))
      welcomeState = 0
      setWelcomeTimeout()
      lastCatchup = 0
    }
    else{
      x.target.close()
    }
  })
  ws.onmessage = (x=>{
    if(!connected || x.target!==ws){
      x.target.close()
      return;
    }
    alone = false
    console.log("received message", x.data)
    x.data.arrayBuffer().then(x=>recieveMessage(new Uint8Array(x)))
  })
  ws.onclose = (x=>{
    console.log("connection closed")
    if(x.target===ws){
      connected=false
      inGame.style.display="none"
      noConn.style.display=""
    }
  })
  ws.onerror = (x=>console.log("connection error",x))
}
connect(params["multiplayer"])

function recieveMessage(arr){
  console.log(arr)
  //return;
  switch (arr[0]) {
    case N_JOIN:
      if(myType==3){
        if(arr[1]==1 || arr[1]==2) myType&=~arr[1]
        else myType = Math.random()>0.5?1:2;
      }
      ws.send(new Uint8Array([N_WELCOME,myType,welcomeState]))
      if (welcomeState<124) welcomeState+=2
      break;
    case N_WELCOME:
      if(welcomeState<126 && (arr[2]&-2)>=(welcomeState&-2)){
        console.log(myType)
        myType&=~arr[1]
        console.log(myType)
      }
      break;
    case N_CATCHUP:
      if(welcomeState<126 && !(welcomeState&1)){
        setWelcomeTimeout()
        if(arr[1]+arr[2]*256==lastCatchup+1){
          recieveMessage(arr.slice(3))
          lastCatchup+=1
        }
      }
      break;
    case N_CATCHUPDONE:
      if(welcomeState<126) welcomeState|=1
      break;
    case N_MOVE:
      addMove(...arr.slice(1),true)
      break;
    case N_UNDO:
      undo(true)
      break;
    case N_SUBMIT:
      submit(true)
      break;
    case N_RESET:
      if(confirm("do you want to reset?")){
        oldReset()
        history=[]
      }
      break;
    default:
      console.log("incomprehensible message",arr)
  }
}

oldSubmit = submit
submit = function(nosend){
  if(!nosend && !hasPermission())return false;
  oldSubmit()
  history.push(new Int8Array([N_SUBMIT]))
  if(!nosend)ws.send(history[history.length-1])
}
oldMove = addMove
addMove = function(l1,t1,x1,y1, l2,t2,x2,y2,nosend){
  if(!nosend && !hasPermission())return false;
  oldMove(l1,t1,x1,y1, l2,t2,x2,y2)
  history.push(new Int8Array([N_MOVE,l1,t1,x1,y1, l2,t2,x2,y2]))
  if(!nosend)ws.send(history[history.length-1])
}
oldUndo = undo
undo = function(nosend){
  if(!nosend && !hasPermission())return false;
  if(oldUndo()){
    history.pop()
    if(!nosend)ws.send(new Int8Array([N_UNDO]))
  }
}
oldReset = reset
reset = function(nosend){
  if(myType>0){
    oldReset()
    history=[]
    if(!nosend)ws.send(new Int8Array([N_RESET]))
  } else {
    alert("cannot reset as an observer")
  }
}
oldTestLegal = testLegal
testLegal = function(newCol){
  oldTestLegal(newCol)
  isTurn.innerText=(col?"Black":"White")+(hasPermission()?"(you)":"")+" to play"
  if(!hasPermission()){
    undobtn.disabled=true
    submitbtn.disabled=true
  }
}

function popCount(n){
  n = (n&0x55) + ((n&0xaa)>>1)
  n = (n&0x33) + ((n&0xcc)>>2)
  return (n&0xf0) + (n&0x0f)
}

//connect("test")
