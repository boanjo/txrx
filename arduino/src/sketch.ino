
//
// Daten von Oregon Scientific Weather Station (www.arduino-praxis.ch)
// Anzeige auf Nokia LCD
// Datei: project_oregon_wetterstation_anzeige.ino
// Datum: 18.08.12

// Anpassungen
// LED Status auf PortB Pin1 (D09)
// 
// Anschlüsse Arduino
// -------------------
// D09: LED Empfang
// D08: Data von RF433 Receiver
// D07: Display CE (CS)
// D06: Display RST
// D05: Display DC
// D04: Display DIN
// D03: Display CLK



/***
 * Arduino Oregon Scientific V3 Sensor receiver v0.3
 * Updates: http://www.lostbyte.com/Arduino-OSV3
 * Contact: brian@lostbyte.com
 * 
 * Receives and decodes 433.92MHz signals sent from the Oregon Scientific
 * V3 sensors: THGN801 (Temp/Humid), PCR800 (Rain), WGR800 (Wind), UVN800 (UV)
 * For more info: http://lostbyte.com/Arduino-OSV3
 *
 * Hardware based on the Practical Arduino Weather Station Receiver project
 * http://www.practicalarduino.com/projects/weather-station-receiver
 * 
 * Special thanks to Kayne Richens and his ThermorWeatherRx code.  
 * http://github.com/kayno/ThermorWeatherRx
 *
 */

#include <NewRemoteReceiver.h> 
#include <NewRemoteTransmitter.h> 

// LCD Objekt erstellen

#define INPUT_CAPTURE_IS_RISING_EDGE()    ((TCCR1B & _BV(ICES1)) != 0)
#define INPUT_CAPTURE_IS_FALLING_EDGE()   ((TCCR1B & _BV(ICES1)) == 0)
#define SET_INPUT_CAPTURE_RISING_EDGE()   (TCCR1B |=  _BV(ICES1))
#define SET_INPUT_CAPTURE_FALLING_EDGE()  (TCCR1B &= ~_BV(ICES1))

// Control LED on pin 9
#define LED_ON()         ((PORTB &= ~(1<<PORTB1)))
#define LED_OFF()        ((PORTB |=  (1<<PORTB1)))

// Reset RX state data
#define WEATHER_RESET()             {short_count = packet_bit_pointer = 0; weather_rx_state = RX_STATE_IDLE; current_bit = BIT_ZERO;  LED_OFF(); }

// Pulse Widths.  Each sensor is a little different, but they all fall in this range
#define SHORT_PULSE_MIN				50  	// 200us
#define SHORT_PULSE_MAX				189		// 758us
#define LONG_PULSE_MIN				190		// 760us
#define LONG_PULSE_MAX				330		// 1650us

// Preamble
#define SYNC_COUNT					20

// Rx States
#define RX_STATE_IDLE               0
#define RX_STATE_RECEIVING          1
#define RX_STATE_PACKET_RECEIVED    2

#define BIT_ZERO                    0
#define BIT_ONE                     1

typedef unsigned int       uint; 
typedef signed int         sint; 

struct NewCode {
  unsigned long address;
  byte unit;
  boolean groupBit;
  boolean on;
};

#define MAX_REC_CODES       8

NewCode recCodeSwitch[MAX_REC_CODES];
byte recCodeSwitchCnt = 0;
byte recCodeSwitchIndex = 0;
byte recCodeSwitchSendIndex = 0;
byte recCnt = 0;
byte sendCnt = 0;
uint captured_time;
uint previous_captured_time;
uint captured_period;
uint current_bit;
uint packet_bit_pointer;
uint short_count;
uint weather_rx_state;

String inputString = "";         // a string to hold incoming data

boolean previous_period_was_short = false;
byte packet[25];

double   temperature = 0.0;
int      humidity = 0;
int      channel = 0xff;
double   windGust = 0.0;
double   windAvg = 0.0;
int      uv      = 0;
double   rain_bucket_tips = 0;
double   rain_total = 0;
double   rain_rate = 0;

const char* switchStr = "device";


#define LED_DEVICE_SEND     7
#define LED_DEVICE_RECEIVE  6
#define LED_TEMP            5
#define LED_RAIN            4


void setup()
{

  pinMode(LED_DEVICE_SEND, OUTPUT);
  pinMode(LED_DEVICE_RECEIVE, OUTPUT);
  pinMode(LED_TEMP, OUTPUT);
  pinMode(LED_RAIN, OUTPUT);

  NewRemoteReceiver::init(0, 1, recCodeCallback);

  Serial.begin(115200);
  

  DDRB = 0x2F;   
  DDRB  &= ~(1<<DDB0); 
  PORTB &= ~(1<<PORTB0);
  DDRD  |=  B11000000;
  TCCR1A = B00000000;
  TCCR1B = ( _BV(ICNC1) | _BV(CS11) | _BV(CS10) );
  SET_INPUT_CAPTURE_RISING_EDGE();
  TIMSK1 = ( _BV(ICIE1) | _BV(TOIE1) );

  Serial.println("{info, starting_txrx_firmware}.");
    
  WEATHER_RESET();
}

// Callback function is called only when a valid code is received.
void recCodeCallback(NewRemoteCode receivedCode) {
  // Note: interrupts are disabled. You can re-enable them if needed.

  if(recCodeSwitchCnt >= MAX_REC_CODES)
  {
    Serial.println("{error, send_queue_full}.");
  }
  else
  {
    recCodeSwitch[recCodeSwitchIndex].address = receivedCode.address;
    recCodeSwitch[recCodeSwitchIndex].unit = receivedCode.unit;
    recCodeSwitch[recCodeSwitchIndex].groupBit = receivedCode.groupBit;
    if(receivedCode.switchType == NewRemoteCode::off)
      recCodeSwitch[recCodeSwitchIndex].on = false;
    else
      recCodeSwitch[recCodeSwitchIndex].on = true;
    recCodeSwitchCnt++;
    recCnt++;
    recCodeSwitchIndex = (recCodeSwitchIndex + 1) % MAX_REC_CODES;
  }
}
void loop() 
{
  char inChar;

  // weather packet ready to decode
  if(weather_rx_state == RX_STATE_PACKET_RECEIVED) 
  {
    //  Shift bits right 1
    for (int j = packet_bit_pointer/8+1; j > 0; j--)
    {
      packet[j] >>= 1;
      if ((packet[j - 1] & 0x01) == 1)
	packet[j] |= (1 << (7));
    }
    packet[0] >>= 1;

    switch(packet[0])
    {
    case 0x5F: DecodeTemp(); break;
      //case 0x5B: DecodeUV(); break;
      //case 0x58: DecodeWind(); break;
    case 0x54: DecodeRain(); break;
    }

    WEATHER_RESET();
  }

  if(sendCnt != recCnt)
  {
    digitalWrite(LED_DEVICE_RECEIVE, HIGH);
    Serial.print("{device, {action,");
    Serial.print(recCodeSwitch[recCodeSwitchSendIndex].on);
    Serial.print("}, {address,");
    Serial.print(recCodeSwitch[recCodeSwitchSendIndex].address);
    Serial.print("}, {unit,");
    Serial.print(recCodeSwitch[recCodeSwitchSendIndex].unit);
    Serial.print("}, {group_bit,");
    Serial.print(recCodeSwitch[recCodeSwitchSendIndex].groupBit);
    Serial.println("}}.");
    recCodeSwitchSendIndex = (recCodeSwitchSendIndex + 1) % MAX_REC_CODES;
    recCodeSwitchCnt--;
    sendCnt++;
    digitalWrite(LED_DEVICE_RECEIVE, LOW);
  }

 
  if (Serial.available() > 0) 
  {
    // read the incoming byte:
    inChar = Serial.read();
		
    if (inChar == '{') 
    {
      inputString = "";
    } 
    else if (inChar == '}') 
    {
      if(inputString.length() > 0) {
	Serial.print("{cmd,"); Serial.print(inputString); Serial.println("}.");
            
	long time = millis();
	int commaPosition = inputString.indexOf(',');
	if(commaPosition != -1) {
	  String cmd = inputString.substring(0, commaPosition);
	  inputString = inputString.substring(commaPosition+1, inputString.length());
	  commaPosition = inputString.indexOf(',');
	  String action = inputString.substring(0, commaPosition);
	  inputString = inputString.substring(commaPosition+1, inputString.length());
	  commaPosition = inputString.indexOf(',');
	  String address = inputString.substring(0, commaPosition);
	  inputString = inputString.substring(commaPosition+1, inputString.length());
	  commaPosition = inputString.indexOf(',');
	  String unit = inputString.substring(0, commaPosition);

	  if(cmd.indexOf(switchStr) != -1) {
                          
	    char tarray[16]; 
	    address.toCharArray(tarray, address.length()+1);

	    // Serial.println(address);
	    //Serial.println(atol(tarray));
                           
	    digitalWrite(LED_DEVICE_SEND, HIGH);

	    NewRemoteTransmitter transmitter(atol(tarray), 11, 260, 3);
	    if(action.indexOf("on") != -1) {
	      // {new,on,38129658,15}
	      transmitter.sendUnit(unit.toInt(), true);
	    } else {
	      transmitter.sendUnit(unit.toInt(), false);
	    }
        
	    digitalWrite(LED_DEVICE_SEND, LOW);
                   
	  }

  
	}

	long diff = millis() - time;
        Serial.print("{cmd_time,");        
	Serial.print(diff);
	Serial.println("}.");
      }

      inputString = "";
    } 
    else
    {
      // add it to the inputString:
      inputString += inChar;

    }
  }

  
} 


// Overflow interrupt vector
ISR(TIMER1_OVF_vect)
{
}

// ICR interrup vector
ISR(TIMER1_CAPT_vect)
{ 
  captured_time = ICR1;

  if(INPUT_CAPTURE_IS_RISING_EDGE())
    SET_INPUT_CAPTURE_FALLING_EDGE();
  else 
    SET_INPUT_CAPTURE_RISING_EDGE();

  captured_period = (captured_time - previous_captured_time);
  if(weather_rx_state == RX_STATE_IDLE) 
  {
    if(((captured_period >= SHORT_PULSE_MIN) && (captured_period <= SHORT_PULSE_MAX)))
      short_count++;    
    else if((captured_period >= LONG_PULSE_MIN) && (captured_period <= LONG_PULSE_MAX))
    { 
      if(short_count > SYNC_COUNT) 
      {
	weather_rx_state = RX_STATE_RECEIVING;
	LED_ON();
      } 
      else 
	WEATHER_RESET();
    } 
    else 
      WEATHER_RESET();
  } 
  else if(weather_rx_state == RX_STATE_RECEIVING) 
  {    
    if((captured_period >= SHORT_PULSE_MIN) && (captured_period <= SHORT_PULSE_MAX)) 
    { 
      if(previous_period_was_short) 
      {      
	if(current_bit == BIT_ONE) 
	  packet[packet_bit_pointer >> 3] |=  (0x80 >> (packet_bit_pointer&0x07));
	else if (current_bit == BIT_ZERO) 
	  packet[packet_bit_pointer >> 3] &= ~(0x80 >> (packet_bit_pointer&0x07));

	packet_bit_pointer++;
	previous_period_was_short = false;
      } 
      else 
	previous_period_was_short = true;
    } 
    else if((captured_period >= LONG_PULSE_MIN) && (captured_period <= LONG_PULSE_MAX)) 
    {    
      current_bit = !current_bit;

      if(current_bit == BIT_ONE) 
	packet[packet_bit_pointer >> 3] |=  (0x80 >> (packet_bit_pointer&0x07));
      else if (current_bit == BIT_ZERO) 
	packet[packet_bit_pointer >> 3] &= ~(0x80 >> (packet_bit_pointer&0x07));

      packet_bit_pointer++;
    }
    else
    {
      if(packet_bit_pointer > 40) 
	weather_rx_state = RX_STATE_PACKET_RECEIVED;
      else
	WEATHER_RESET();
    }
  }

  previous_captured_time = captured_time;
}




// THGN801 Temperature / Humidity Sensor Data Format.
// Sample Data:
// 01011111000101000010100001000000100011001000000000001100101000001011010001111001
// --------------------------------4444333322221111SSSS66665555--------CCCC--------
// 
// 11112222.33334444 = Temperature
// 55556666 = Humidity %
// CCCC = CRC
void DecodeTemp()
{
  digitalWrite(LED_TEMP, HIGH);

  // check the packet CRC
  if (!ValidCRC(16))
  {
    Serial.print("{warning, temp_crc, [");
    byte test;
    for (int x = 0; x < 16; x++)
    {
      test = GetNibble(x);
      Serial.print("'");
      Serial.print(test, HEX);
      Serial.print("', ");
    }

    test = GetNibble(16);
    Serial.print("'");
    Serial.print(test, HEX);
    Serial.print("'");
    
    Serial.println("]}.");
    return;
  }
    
  // grab the channel
  channel = GetNibble(5);
  
  // grab the temperature
  int whole =  (GetNibble(11) * 10) + GetNibble(10);
  int decimal = (GetNibble(9) * 10) + GetNibble(8);
  temperature = (decimal * 0.01) + whole;
    
  if (GetNibble(12) != 0)
    temperature *= -1;
        
  // convert Celsius to Fahrenheit 
  //temperature = ((9.0 / 5.0) * temperature) + 32;
        
  // grab the humidity
  humidity = (GetNibble(14) * 10) + GetNibble(13);
   
  Serial.print("{temperature, {ch,");
  Serial.print(channel);
  Serial.print("},{value,");
  Serial.print(temperature);
  Serial.println("}}.");

  Serial.print("{humidity, {ch,");
  Serial.print(channel);
  Serial.print("}, {value,");
  Serial.print(humidity);
  Serial.println("}}.");

  digitalWrite(LED_TEMP, LOW);
    
}

// UVN800 UV Sensor
// Sample Data:
// 010110110001111000101000101100100000000000001000111000000101110001110000
// ------------------------------------11111----------------CCCC-----------
// 
// 1111 = UV
// CCCC = CRC
void DecodeUV()
{
  // check the packet CRC
  if (!ValidCRC(14))
  {
    Serial.println("UV: CRC Error!");
    return;
  }
    
  // Grab the UV value
  uv = GetNibble(9);
    
  Serial.print("UV: ");
  Serial.println(uv);
}

// WGR800 Wind speed sensor
// Sample Data:
// 0101100010010001001000000010010000001011000000110100100000001110000000001111110011101010
// --------------------------------xxxxDDDDxxxxxxxxG2G2G1G1xxxxA2A2A1A1----CCCC------------
//
//DDDD = Direction
//G1.G2 = Gust Speed (m per sec)
//A1.A2 = Avg Speed(m per sec)
const char windDir[16][4] = {  "N  ", "NNE", "NE ", "ENE",  "E  ", "ESE", "SE ", "SSE",  "S  ", "SSW", "SW ", "WSW",  "W  ", "WNW", "NW ", "NNW"};
void DecodeWind()
{
  // check the packet CRC
  if (!ValidCRC(18))
  {
    Serial.println("Wind: CRC Error!");
    return;
  }
    
  // Wind Direction
  int wind_dir = GetNibble(9);
    
  // Gust speed
  windGust = ((GetNibble(13) * 100) + GetNibble(12)) * .01;
    
  // Avg speed
  windAvg = ((GetNibble(16) * 100) + GetNibble(15)) * .01;
     
  // Convert meter per sec to MPH
  windGust = windGust * 2.23693181818;
  windAvg = windAvg * 2.23693181818;    
    
  Serial.print("Wind Gust: ");
  Serial.print(windGust);
    
  Serial.print("    Wind Avg: ");
  Serial.print(windAvg);
    
  Serial.print("    Dir: ");
  Serial.println(windDir[wind_dir]);
    
    
    
}

// PCR800 Rain Guage
// Sample Data:
//010101001001100000100000001110010000101010000000000000010110001011100000000000100010100000010000 
//--------------------------------BBBBAAAA999988887777666655554444333322221111CCCC----------------
//
// 11112223333.444455556666 = Total Rain Fall (inches)
// 77778888.9999AAAABBBB = Current Rain Rate (inches pre hour)
// CCCC = CRC
void DecodeRain()
{
  digitalWrite(LED_RAIN, HIGH);

  // check the packet CRC
  if (!ValidCRC(19))
  {
    Serial.print("{warning, rain_crc, [");
    byte test;

    for (int x = 0; x < 19; x++)
    {
      test = GetNibble(x);
      Serial.print("'");
      Serial.print(test, HEX);
      Serial.print("', ");
    }

    test = GetNibble(19);
    Serial.print("'");
    Serial.print(test, HEX);
    Serial.print("'");

    Serial.println("]}.");
    return;
  }
    
  // grab the channel
  channel = GetNibble(5);
  
  double total = 0;
  for (int x = 0; x < 6; x++)
  {
    total = total* 10;
    total += GetNibble(18-x);
  }
  //rain_total = total * .001;
  rain_total = total * .0254;
        
  total = 0;
  for (int x = 0; x < 4; x++)
  {
    total = total* 10;
    total += GetNibble(12-x);
  }
  //rain_rate = total * .01;


  rain_bucket_tips = total;

  Serial.print("{rain, {ch,");
  Serial.print(channel);
  Serial.print("}, {total,");
  Serial.print(rain_total);
  Serial.print("}, {tips,");
  Serial.print(rain_bucket_tips);
  Serial.println("}}.");
         
  digitalWrite(LED_RAIN, LOW);
}

// CRC = the sum of nibbles 1 to (CRCpos-1);
bool ValidCRC(int CRCPos)
{
  bool ret = false;
  int start = 1;
  byte check = GetNibble(CRCPos);
  byte crc = 0;
  for (int x = start; x < CRCPos; x++)
  {
    byte test = GetNibble(x);
    crc += test;
  }

  int tmp = ((byte)(crc) & (byte)(0x0f));

  if (tmp == check)
    ret = true;

  return ret;
}

// Grab nibbile from packet and reverse it
byte GetNibble(int nibble)
{
  int pos = nibble / 2;
  int nib = nibble % 2;
  byte b = Reverse(packet[pos]);
  if (nib == 1)
    b = (byte)((byte)(b) >> 4);
  else
    b = (byte)((byte)(b) & (byte)(0x0f));
        
  return b;
}

// Reverse the bits
byte Reverse(byte b) 
{ 
  int rev = (b >> 4) | ((b & 0xf) << 4); 
  rev = ((rev & 0xcc) >> 2) | ((rev & 0x33) << 2);
  rev = ((rev & 0xaa) >> 1) | ((rev & 0x55) << 1); 
  return (byte)rev; 
}



