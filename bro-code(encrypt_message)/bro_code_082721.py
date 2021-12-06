#!/usr/bin/env python3

class base64:
  def __init__(self, number = None, alphabet = None):
    self.number, self.alphabet = number, alphabet
  def encode(self):
    """Converts an base10 integer to a base_? string."""
    number, alphabet = int(self.number), str(self.alphabet)
    base64, sign = '', ''
    if number < 0:
        sign, number = '-', -number
    if 0 <= number < len(alphabet):
        return sign + alphabet[number]
    while number != 0:
        number, i = divmod(number, len(alphabet))
        base64 = alphabet[i] + base64
    return sign + base64
  def decode(self):
    number, alphabet = self.number, self.alphabet
    sign, number, alphabet = '', str(number), str(alphabet)
    if number[0] == '-':
      sign, number = '-', number[1:]
    char, index = [x for x in alphabet], list( range(0, len(alphabet)) )
    base64_decode_dict = dict(zip(char, index))
    c = 0
    for key_1, value_1 in enumerate(number):
      c += int(base64_decode_dict[value_1]) *  64**(len(number) - 1 - key_1)
    return int(sign + str(c))

def base64_key_generator():
  from random import sample
  alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
  l = sample(list(range(0, len(alphabet))), len(alphabet))
  keys = ''
  for i in range(0, len(alphabet)): 
    keys += alphabet[l[i]]
  return keys

def base_encode(number, base): # 2 <= base <= 36
  if 2 <= base <= 36:
    alphabet = '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ'[:base]
  base2, sign = '', ''
  if number < 0:
      sign, number = '-', -number
  if 0 <= number < len(alphabet):
      return sign + alphabet[number]
  while number != 0:
      number, i = divmod(number, len(alphabet))
      base2 = alphabet[i] + base2
  return sign + base2

morse_code_dict = {'A':'.-', 'B':'-...', 'C':'-.-.', 'D':'-..', 'E':'.', 'F':'..-.', 'G':'--.', 
                   'H':'....', 'I':'..', 'J':'.---', 'K':'-.-', 'L':'.-..', 'M':'--', 'N':'-.',
                   'O':'---', 'P':'.--.', 'Q':'--.-', 'R':'.-.', 'S':'...', 'T':'-',
                   'U':'..-', 'V':'...-', 'W':'.--', 'X':'-..-', 'Y':'-.--', 'Z':'--..',
                   '1':'.----', '2':'..---', '3':'...--', '4':'....-', '5':'.....', 
                   '6':'-....', '7':'--...', '8':'---..', '9':'----.', '0':'-----', 
                   ',':'--..--', '.':'.-.-.-', '?':'..--..', '/':'-..-.', '-':'-....-', '(':'-.--.', 
                   ')':'-.--.-', '@': '.--.-.', '$': '...-..-', '"': '.-..-.', '_': '..--.-', '+': '.-.-.', 
                   '=': '-...-', ';': '-.-.-.', ':': '---...', '&': '._...', '!': '-.-.--', '#': '-..-..',
                   "'": '.----.', '’': '.----.'}

def morse_code_inv(morse_code): # this returns a corresponding alphabet by morse_code
  return list(morse_code_dict.keys())[list(morse_code_dict.values()).index(morse_code)]

def Char2CodeNum(code):
  return '0'.join([x for x in morse_code_dict[code]]).replace('-', '111').replace('.', '1')

def encrypt(message, key):
  c = ''
  for i in message.split(' '):
    c += '0000' 
    for j in [x.upper() for x in i]:
      c += Char2CodeNum(j) + '000'
  c = c[4:-3]
  c = int(c, base = 2) # base2 to base10 (base_decode)
  c = base64(number = c, alphabet = key).encode() 
  return c

def decrypt(message, key):
  c = base64(number = message, alphabet = key).decode() 
  c = base_encode(c, base = 2) # base10 to base2
  m = ''
  for i in c.split('0000000'):
    m += ' '
    for j in i.split('000'):
      m += morse_code_inv(j.replace('111', '-').replace('1', '.').replace('0', ''))
  return m[1:]

# Hard-coded driver function to run the program
def main():
  ask1 = input('Enter message or code (Required): ')
  ask2 = input('Encrypt or Decrypt? (Required): ')
  while ask2.upper() != 'ENCRYPT' and ask2.upper() != 'DECRYPT' and ask2.upper() != 'E' and ask2.upper() != 'D':
    print('Please type either "encrypt" or "decrypt" only.')
    ask2 = input('Encrypt or Decrypt? (Required): ')
  if ask2.upper() == 'ENCRYPT' or ask2.upper() == 'E':
    base64_keys = input('Enter base64 key (Optional): ')      
    if base64_keys == '':
      base64_keys = base64_key_generator()
    else:
      while len(base64_keys) != len(set(base64_keys)) and len(base64_keys) != 64 or ''.join(sorted(base64_keys)) != '+/0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz':
        print('Please enter a valid base64 key!!!')
        base64_keys = input('Enter base64 key (Optional): ')
        if base64_keys == '': 
          base64_keys = base64_key_generator()
          break
    print(f'\nCode: {encrypt(message = ask1, key = base64_keys)}\nBase64 Key: {base64_keys}\n')
  elif ask2.upper() == 'DECRYPT' or ask2.upper() == 'D':
    try:
      base64_keys = input('Enter base64 key (Required): ')
      while len(base64_keys) != len(set(base64_keys)) and len(base64_keys) != 64 or ''.join(sorted(base64_keys)) != '+/0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz':
        print('Please enter a valid key!!!')
        base64_keys = input('Enter base64 key (Required): ')
      print(f'\nMessage: {decrypt(ask1, base64_keys)}\n')    
    except:
      print(f'Messages cannot be decrypted. \nIf you were actually trying to encrypt the messages, then\n\nCode: {encrypt(message = ask1, key = base64_keys)}\nBase64 Key: {base64_keys}\n')

# Executes the main function
if __name__ == '__main__':
  while True: main()
