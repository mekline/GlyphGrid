//lets display the current time
var s = -1;
function displayTime() {
  s++
  
  //add zero to the left of the numbers if they are single digits
  if(s <= 9) s = '0'+s;
  //set background color
  if (s<15) {
  document.body.style.background = '#00CC00';
  } else if (s<20) {
  document.body.style.background = '#FF9933';
  } else { document.body.style.background = '#CC3300'}
  //set time
  document.getElementById("hex").innerHTML = s;
  
  //retrigger the function every second
  setTimeout(displayTime, 1000);
}

//call the function
displayTime();