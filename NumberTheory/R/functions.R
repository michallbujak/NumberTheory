#'  Print pasted
#'
#'  The function allows user to print pasted, using paste0, text with no nead of writing paste.
#'
#' @param x elements, separated with coma
#' @return pasted text
#' @export
printt = function(x, ...){
  cat(x, ..., sep = '')
  cat('\n')
}



#' Check if the element is an integer
#'
#' Built in function is.int checks only format of the number, not if it is actually integer.
#' This function checks if it is really an integer.
#'
#' @param x object to be checked
#' @return boolean value
#' @export
is.int = function(x){
  if(!is.numeric(x)) return(FALSE)
  return((round(x) == x))
}



#' Check if all are integers
#'
#' Using funcion is.int from this package, checks if all given elements are actual integers.
#'
#' @param x,... objects to be checked
#' @return boolean value
#' @export
all.int = function(x, ...){
  for(i in c(x, ...)){
    if(!is.int(i)) return(FALSE)
  }
  return(TRUE)
}



#' Calculate modulus value
#'
#' Equivalent of %%, however much more transparent
#' For given equation x = a mod n
#'
#'
#' @param a number to be calculated
#' @param n modulus
#' @return
#' @export
mod = function(a, n){
  if(!(is.int(a) & is.int(n))) stop('given number is not an integer')
  return(a %% n)
}



#' Euclidean division
#'
#' Performs Euclidean division on given entries
#'
#' @param a divident
#' @param b divisor
#' @return two-element vector. First quotient, next reminder
#' @export
euclidean.div = function(a, b){
  rem = mod(a,b)
  res = (a-rem)/b
  return(c(res,rem))
}



#' GCD
#'
#' Algorithm used to find the greatest common divisor
#'
#' @param a,b arguments for GCD function
#' @return GCD of the two numbers
#' @export
GCD = function(a, b){
  if(!(is.int(a) & is.int(b))) stop('given number is not an integer')
  if (a == 0)  return(b)
  return(GCD(b %% a, a))
}



#' Euler's Phi
#'
#' Calculates Euler's totient function's value for given entry
#'
#' @param n argument
#' @return Value of Euler's totient
#' @export
eulersPhi = function(n){
  if(!is.int(n)) stop('given number is not an integer!')
  if(n < 1) stop('given number is non-positive!')
  if(n == 1) return(1)
  if(n == 2) return(1)
  result = n
  for(p in 2:n){
    if(p*p > n) break
    if(n %% p == 0){
      while (n %% p == 0)
        n = n/p
      result = result - result/p
    }
  }
  if (n > 1)
    result = result - result/n
  return(result)
}



#' Find first primeroot
#'
#' Functions looks for the first primeroot for given n
#'
#' @param n argument
#' @return first prime root
#' @export
find.primeroot = function(n){
  if(!is.int(n)) stop('given number is not an integer!')
  if(n < 1) stop('given number is non-positive!')
  if(n < 2) stop('n is lesser then 2!')
  rzad = eulersPhi(n)
  for(i in 2:n){
    if(GCD(i,n) != 1) next
    temp = i
    temp0 = i
    for(j in 2:rzad){
      temp = mod(temp*temp0,n)
      if(mod(temp,n) == 1 & j < rzad){
        break
      }
      if(j == rzad){
        return(i)
      }
    }
  }
  message('Prime root does not exist')
  return(FALSE)
}



#' Order modulo
#'
#' Function calculates order of the element in cyclic group Zn
#'
#' @param x element which order is yet to be calculated
#' @param n modulus
#' @param disp displays additional information if number is the prime root. By default set to FALSE
#' @return order of the element
#' @export
ord = function(x,n, disp = FALSE){
  if(!is.int(n)) stop('given number n is not an integer!')
  if(n < 1) stop('given number n is non positive!')
  if(!is.int(x)) stop('given number x is not an integer!')
  if(x < 1) stop('given number x is non-positive!')
  if(x == 1){
    return(1)
  }
  rzad = eulersPhi(n)
  x = mod(x,n)
  x0 = x
  for(i in 2:(rzad-1)){
    x = mod(x*x0,n)
    if(x == 1){
      return(i)
    }
  }
  if(disp) printt(paste0('It is the prime root (order = ', i+1, ')'))
  return(i+1)
}



#' Factorise number
#'
#' Factorises given number giving prime factors. First it shows number
#' of the prime factors, next sequence prime factor followed by its multiplicity
#'
#' @param n number to be factorised
#' @return data frame
#' @export
prime.factors = function(n){
  if(!is.int(n)) stop('given number is not an integer!')
  if(n < 1) stop('given number is non-positive!')
  result = c(0)
  temp = n
  temp0 = 1
  for(i in 2:(n-1)){
    if(temp %% i == 0){
      temp0 = temp0 + 2
      result = append(result, i)
      result = append(result, 0)
      while(temp %% i == 0){
        temp = temp/i
        result[temp0] = result[temp0] + 1
      }
    }
  }
  result[1] = (length(result) - 1)/2
  if(length(result) == 1){
    result[1] = 1
    result[2] = n
    result[3] = 1
    printt('Given number is prime')
  }
  df = data.frame('distinct factors' = result[1])
  df0 = data.frame(t(result[-1]))
  names(df0) =  rep(c('factor', 'multip.'), ncol(df0)/2)
  df = cbind(df, df0)
  return(df)
}



#' Check if number is prime
#'
#' Checks if given number is prime number with brute force method
#'
#' @param n number to be checked
#' @return boolean value
#' @export
is.prime = function(n){
  if(n == 2) return(TRUE)
  temp = floor(sqrt(n)) + 1
  for(i in 2:temp){
    if(n %% i == 0) return(FALSE)
  }
  return(TRUE)
}



#' Checks coprimity of numbers
#'
#' Function checks whether given numbers are coprime.
#'
#' @param a,b given numbers
#' @return boolean value
#' @export
coprime = function(a,b){
  if(!(is.int(a) & is.int(b))) stop('Given entries are not integers!')
  return(GCD(a,b) == 1)
}



#' Extended GCD
#'
#' Extended algorithm looking for the greatest common divisor.
#' Finds solution for the equation ax + by = 0
#' Allows user to see results step-by-step.
#'
#' @param a,b given entries
#' @param disp choose whether to see partial results. Def. FALSE
#' @return vector consisting of GCD followed by x, y such that ax + by = 0
#' @export
GCD.ext = function(a,b, disp = FALSE){
  if(!is.int(a)) stop('given number a is not an integer!')
  if(!is.int(b)) stop('given number b is not an integer!')
  old_r = a; r = b
  old_s = 1; s = 0
  old_t = 0; t = 1
  while(r != 0){
    quotient = floor(old_r/r)
    temp_r = r; r = old_r - quotient*temp_r; old_r = temp_r
    temp_s = s; s = old_s - quotient*temp_s; old_s = temp_s
    temp_t = t; t = old_t - quotient*temp_t; old_t = temp_t
  }
  if(disp) printt('Vector: gcd, s, t   (s*a + t*b = 1)')
  return(c(old_r, old_s, old_t))
}



#' Modulus inversion
#'
#' Find modulo inversion of given number.
#'
#' @param a will be calcualted a^-1
#' @param n modulus
#' @return inverse of a modulo n
#' @export
mod.inv = function(a, n){
  if(!is.int(n)) stop('given number is not an integer!')
  if(n < 1) stop('given number is non-positive!')
  if(!is.int(a)) stop('given number a is not an integer!')
  a = mod(a,n)
  if(GCD(a,n) != 1) stop('Given numbers are not co-prime!')
  return(GCD.ext(a,n)[2] %% n)
}



#' Power funcion in terms of modulus calculations
#'
#' Allows user to find great powers of numbers. Here is used the algorithm
#' of so-called fast powering. User can see stepwise algorithm's results, by default FALSE.
#'
#' @param x number to be powered
#' @param k power
#' @param n modulus
#' @param disp choose whether to see stepwise results. Def. FALSE
#' @return number, the result
#' @export
mod.power = function(x,k,n, disp = FALSE){
  if(!is.int(n)) stop('given number n is not an integer!')
  if(n < 1) stop('given number n is non-positive!')
  if(!is.int(k)) stop('given number k is not an integer!')
  if(k < 0) stop('given number k is non-positive!')
  if(!is.int(x))('given number x is not an integer!')
  if(k==1) return(x)
  if(k==0) return(1)
  if(disp) printt('Exponentiation by squaring')
  x = mod(x, n)
  j = k
  pots = c()
  while(j != 1 & j != 0){
    if(j %% 2 == 0){
      new_j = j/2
      if(disp) printt(paste0(j, ' = 2 * ', new_j))
      j = new_j
      pots = append(pots, c(1,0))
    } else{
      new_j = (j-1)/2
      if(disp) printt(paste0(j, ' = 2 * ', new_j, ' + 1'))
      j = new_j
      pots = append(pots, c(1,1))
    }
  }
  t = 1
  x_temp = x
  for(i in (length(pots)/2):1){
    t = 2*t
    x_temp = x_temp^2
    x_temp = x_temp %% n
    if(disp) printt(paste0(x, '^', t, ' = ', x_temp))
    if(pots[2*i] == 1){
      t = t + 1
      x_temp = x_temp*x
      x_temp = x_temp %% n
      if(disp) printt(paste0(x, '^', t, ' = ', x_temp))
    }

  }
  return(x_temp)
}


#' Modular linear equation solver
#'
#' Solves linear equation of the form ax = b mod m. If it does not, function returns FALSE.
#' User can see stepwise results.
#'
#' @param a multiplicator of x - the unknown
#' @param b to be equivalent of a*x
#' @param m modulus
#' @param disp choose whether to see the stepwise results
#' @return vector: result,modulus
#' @export
mod.lin.eq = function(a,b,m, disp = FALSE){
  if(!all.int(a,b,m)) stop('Not all entries are integers!')
  if(disp) printt('Solving equation: ', a, 'x = ',b, ' mod ',m)
  a = mod(a,m)
  b = mod(b,m)
  gcdext = GCD.ext(a,m)
  if(!is.int(b/gcdext[1])){
    warning('Equation does not have solutions')
    return(FALSE)
  }
  result0 = mod(gcdext[2]*b,m)/gcdext[1]
  mod0 = m/gcdext[1]
  if(disp) printt('Result: x = ', result0, ' mod ', mod0)
  return(c(result0,mod0))
}


#' Solves system of simple modular equations
#'
#' Solves system of equations given by the chinese reminder theorem.
#'
#' @param y vector of values
#' @param m vector of modulus
#' @param disp choose whether to see stepwise solutions. By def. FALSE
#' @return data frame
#' @export
mod.equations = function(y, m, disp = FALSE){
  vect = c(y,m)
  for(i in vect){
    if(!is.int(i)) stop('Given entries are not integers!')
  }
  vect = c()
  for(i in 1:length(y)){
    if(y[i] == 0){
      vect = append(vect, i)
    }
  }
  y = y[-vect]
  m = m[-vect]
  if(length(y) == 0) return(TRUE)
  if(length(y) == 1) return(c(y[1]), m[1])
  for(i in 1:(length(m)-1)){
    for(j in (i+1):length(m)){
      if(!coprime(m[i], m[j])){
        gcd = GCD(m[i], m[j])
        X = as.integer(unname(prime.factors(gcd)))
        for(k in 1:X[1]){
          for(l in 1:X[2*k+1]){
            t = X[2*k]
            if(is.int(y[i]/t)){
              m[i] = m[i]/t
              y[i] = mod(y[i], m[i])
            }
            if(is.int(y[j]/t)){
              m[j] = m[j]/t
              y[j] = mod(y[j], m[j])
            }
          }
        }
        if(m[i] == m[j] & y[i] != y[j]){
          warning('Equation is contradictory!')
          return(FALSE)
        }
      }
    }
  }
  if(disp){
    printt('Solving system of given equations: ')
    df = data.frame('x', '=', y, 'mod',m)
    df = unname(df)
    print(df, row.names = FALSE)
  }
  mod_equations = function(y,m, disp){
    inv_n = mod.inv(m[2], m[1])
    if(disp){
      printt('Solving subsystem of given equations: ')
      df = data.frame('x', '=', y, 'mod',m)
      df = unname(df)
      print(df, row.names = FALSE)
    }
    result = mod(y[2] + m[2]*mod((y[1]-y[2])*inv_n, m[1]), m[1]*m[2])
    if(disp) printt('Result: ', result, ' mod ', m[1]*m[2])
    return(c(result, m[1]*m[2]))
  }
  while(!length(y) == 2){
    new = mod_equations(y[1:2], m[1:2], disp)
    y = c(new[1], y[-c(1,2)])
    m = c(new[2], m[-c(1,2)])
  }
  return(mod_equations(y,m,disp))
}


#' Shanks' algorithm for discrete logarithm
#'
#' Looks for the discrete logarithm. Base is g - the prime root - preferably; a is power, m modulus.
#' There might be warning passed, for ex. if g is not a prime root.
#' Also user can see stepwise version
#'
#' @param a power
#' @param g base - prime root preferably
#' @param m modulus
#' @param disp choose whether to see stepwise solution
#' @param warnings choose whether to see warnings
#' @return number
#' @export
dlog.shanks = function(a, g, m, disp = TRUE, warnings = TRUE){
  if(disp) printq = function(x){ cat(x); cat('\n') }
  if(!disp) printq = function(x) {}
  if(!all.int(a,g,m)) stop('Some arguments are not integers!')
  if(warnings) if(ord(g, m) != eulersPhi(m)) message('Given g is not giving this cyclic group (probably g is not a prime root)!')
  if(warnings) if(!is.prime(m)) message('Note that given m is not prime which may lead to strange results')
  printq('Algorithm of Shanks baby steps giant steps')
  printq('Input for equation: g^x = a mod m:')
  printq(paste('a =', a, ', g =', g, ', m=', m))
  n = ord(g,m)
  printq(paste('Order of g in Zm equals:', n))
  n = 1+ floor(sqrt(n))
  printq('n = 1 + floor(sqrt(m))')
  printq(paste('Here, n =', n))
  g_inv = mod.inv(g,m)
  if(is.na(g_inv)){
    message('There is no modular inverse G MOD M')
    return(FALSE)
  }
  printq(paste('Calculating g^-1 mod m. Result:', g_inv))
  printq('Creating steps lists')
  printq('(As a reminder, baby steps are: 1,g^1,..,g^n, giant steps are: a,a*g^(-n),a*g^(-2n),...,a*g^(-n^2) )')
  baby_steps = c(1)
  giant_steps = c(a)
  for(i in 1:n){
    baby_steps[i+1] = mod.power(g,i,m)
    giant_steps[i+1] = mod(a*mod.power(g_inv, i*n,m),m)
  }
  df = data.frame(t(baby_steps))
  df = rbind(df, giant_steps)
  colnames(df) = mapply(paste0, 'k=', 0:n)
  rownames(df) = c('g^k', 'a*g^(-k*n)')
  if(disp) print(df)
  printq('Looking for a shared entry in both lists')
  for(i in 1:(n+1)){
    stop = FALSE
    for(j in 1:(n+1)){
      if(df[1,i]==df[2,j]){
        printq(paste('Shared element is found. In the baby steps for k =', i-1, ', in giant steps k =',j-1))
        temp = c(i-1,j-1)
        stop = TRUE
        break
      }
    }
    if(stop){break}
  }
  i = temp[1]
  j = temp[2]
  printq('g^i = a*g^(-jn), where i is value of k for the 1st row and j - value of k for the 2nd row')
  printq(paste('x =', i, '+',j,'*',n))
  x = i+j*n
  printq(paste('The result is x = ', x , 'mod', m))
  return(x)
}



#' Pohling-Hellman discrete logarithm
#'
#' Algorith for discrete logarithm, allows user to see stepwise solution
#' of log g  a   in Zm
#'
#' @param a power
#' @param g base
#' @param m modular
#' @param disp choose whether to see stepwise solution, def. FALSE
#' @return number
#' @export
dlog.ph = function(a,g,m, disp = TRUE){
  if(disp) printq = function(x){ cat(x); cat('\n') }
  if(!disp) printq = function(x) {}
  if(!all.int(a,g,m)) stop('At least one of the entries is not an integer!')
  printq('Pohlig-Hellman algorithm for discrete logarithm')
  printq('Entries for equation g^x = a mod m:')
  printq(paste('a =', a, ', g =', g, ', m=', m))
  n = ord(g,m)
  printq(paste0('Order of g in Z',m ,' is: ', n))
  printq('factorization of n')
  tab = prime.factors(n)
  if(disp) print(tab)
  tab_len = as.integer(tab[1])
  g_tab = c()
  a_tab = c()
  eq = c()
  power2_tab = c()
  pi = as.integer(tab[seq(2, 2*tab_len, 2)])
  ai = as.integer(tab[seq(3, 1+2*tab_len,2)])
  for(i in 1:as.integer(tab[1])){
    power2 = pi[i]^ai[i]
    power = n/power2
    g_tab[i] = mod.power(g, power, m)
    a_tab[i] = mod.power(a, power, m)
    eq[i] = dlog.shanks(a_tab[i],g_tab[i],m, disp, FALSE)
    eq[i] = mod(eq[i], power2)
    power2_tab[i] = power2
  }
  df = data.frame('pi' = pi,
                  'ai (multiplicty)' = ai,
                  'g to power' = g_tab,
                  'a to power' = a_tab,
                  'gi^x = ai' = eq
  )
  if(disp) print(df)
  printq('Here we shall solve system of equations:')
  for(i in 1:tab_len){
    printq(paste('x =', eq[i], 'mod', power2_tab[i]))
  }
  result = mod.equations(eq, power2_tab, disp)
}



#' Pollard's algorithm for discrete logarithm
#'
#' Performs Pollard's algorithm for discrete algorithm. Allows user to see stepwise solution.
#' POllard's algorithm is a probability algorithm, meaning it may not yield result.
#' In arguments there is set upper bound. There are availble two methods of spliitng the set.
#' One is with respect to 3, denoted by "mod", the other is "con" staying for consequntly,
#' meaning that there are split with respect to numeration.
#' Things such as this method are not explained here. I am sorry for possible misunderstanding
#' due to different names of algorithm established by authors.
#'
#' @param a power
#' @param g base
#' @param m modular
#' @param starting_point starting point of the algorithm, def. set to 1
#' @param up_bound upper bound of steps, def. set to 10000
#' @param method choose either "mod" or "con", def. MOD
#' @param disp choose whether to display stepwise, def. TRUE
#' @return vector: number and modulo or information of fiasco
#' @export
dlog.pollard = function(a,g,m, starting_point = 1, up_bound = 10000, method = 'mod',disp = TRUE){
  if(!all.int(a,g,m)) stop('At least one of the entries is not an integer!')
  if(disp){
    printt("Executing Pollard's discrete logarithm algorithm")
    printt('Entries: a = ',a, ' , g = ',g, ', m = ', m, '   for equation g^x = a mod m')
    if(method == 'mod') printt('Chosen method (mod) splits into three groups; indexes with respect to modulo 3 (1/0/2)')
    if(method == 'con') printt('Chosen method (con - consecutive) splits groups such as one is followed by the other')
  }
  if(!(method %in% c('mod', 'con'))) stop('You have chosen not addmissible method! Choose "mod" or "con"')

  n = eulersPhi(m)
  starting_point = mod(as.integer(starting_point),n)
  x = c(starting_point)
  u = c(0)
  v = c(0)

  if(method == 'mod'){
    for(i in 1:up_bound){
      if(mod(x[i],3) == 1){
        x[i+1] = mod(a*x[i],m)
        u[i+1] = mod(u[i]+1,n)
        v[i+1] = v[i]
      }
      if(mod(x[i],3) == 0){
        x[i+1] = mod(x[i]*x[i],m)
        u[i+1] = mod(u[i]*2,n)
        v[i+1] = mod(v[i]*2,n)
      }
      if(mod(x[i],3) == 2){
        x[i+1] = mod(g*x[i],m)
        u[i+1] = u[i]
        v[i+1] = mod(v[i]+1,n)
      }
      if(mod(i,2) == 0){
        if(x[i+1] == x[(i/2)+1]){
          number_of_rows = i
          break
        }
      }
    }
  }

  if(method == 'con'){
    b1 = floor(m/3)
    b2 = floor(2*m/3)
    for(i in 1:up_bound){
      if(x[i] <= b1){
        x[i+1] = mod(a*x[i],m)
        u[i+1] = mod(u[i]+1,n)
        v[i+1] = v[i]
      }
      if(x[i] > b1 & x[i] <= b2){
        x[i+1] = mod(x[i]*x[i],m)
        u[i+1] = mod(u[i]*2,n)
        v[i+1] = mod(v[i]*2,n)
      }
      if(x[i] > b2){
        x[i+1] = mod(g*x[i],m)
        u[i+1] = u[i]
        v[i+1] = mod(v[i]+1,n)
      }
      if(mod(i,2) == 0){
        if(x[i+1] == x[(i/2)+1]){
          number_of_rows = i
          break
        }
      }
    }
  }
  q = number_of_rows
  df = data.frame('i' = q/2, 'xi' = x[q/2 + 1], 'ui' = u[q/2 + 1],'vi' = v[q/2 + 1], 'x2i' = x[q + 1], 'u2i' = u[q + 1], 'v2i' = v[q + 1])
  full_df = data.frame('i'=1:(q/2), 'xi'=x[2:(q/2+1)], 'ui'=u[2:(q/2+1)], 'vi'=v[2:(q/2+1)], 'x2i'=x[seq(3,q+1,by=2)], 'u2i'=u[seq(3,q+1,by=2)], 'v2i'=v[seq(3,q+1,by=2)])
  if(disp) print(full_df, row.names = FALSE)
  if(number_of_rows != up_bound){conv = 'converged'; convTF = TRUE
  } else{ conv = 'not converged'; convTF = FALSE}
  if(disp) printt('Look at the last line. Algorithm ', conv)
  if(convTF) print(df, row.names = FALSE)
  result = mod.lin.eq((df$ui - df$u2i), (df$v2i - df$vi), n, disp)
  return(result)
}



#' Power remainder
#'
#' Checks if solution for equation x^n = a mod m exists with respect to x.
#'
#' @param a equivalent value, alike power in discrete logarithm
#' @param n power
#' @param m modulus
#' @param disp choose whether to display stepwise, def. FALSE
#' @return vector: result, modulo
#' @export
power.rem = function(a,n,m, disp = TRUE){
  if(!all.int(a,n,m)) stop('Not all of given entries are integers!')
  if(disp) printt('Starting algorithm for fiding power reminder')
  if(disp) printt('Given equation: x^',n, ' = ', a, ' mod ', m)
  if(disp) printt("First - Euler's criterion - checking if the solution exists")
  tab = prime.factors(m)

  if(disp) printq = function(x){ cat(x); cat('\n') }
  if(!disp) printq = function(x) {}
  eulers.crit = function(a,n,p,pk){
    a = mod(a, pk)
    printq('Entries x^n = a mod p^k:')
    printq(paste(', a =', a,'n =', n, ', p^k =', pk))
    printq('Checking if... (p,a) = 1')
    printq(GCD(p,a))
    printq('Checking if... d:= (n, eulersPhi(p^k))|a')
    eulr = eulersPhi(pk)
    printq(paste0('eulersPhi(p^k) = ', eulr))
    d = GCD(n,eulr)
    printq(paste0('d = ', d))
    printq(paste('d|a', eulr %% d == 0))
    printq('Checking if... a^(fi(p^k)/n) = 1 mod p^k')
    power = eulr/d
    a_pow = mod.power(a, power, pk)
    printq(paste0('fi(p^k)/n = ', power, '  a^(fi(p^k)/n) = ', a_pow))
    return(a_pow == 1)
  }

  tab = as.integer(tab)
  y = c()
  m_tab = c()
  Y = c()
  M = c()

  for(i in 1:tab[1]){
    p = tab[2*i]
    pk = tab[2*i]^(tab[2*i+1])
    a0 = mod(a, pk)
    if(!eulers.crit(a,n,p, pk)){stop("According to Euler's criterion solution does not exist for given entries")
    } else {printq("According to Euler's law solution exists")}
    if(p == 2 & pk >= 8){
      flag = FALSE
      if(mod(a,8) == 1 | mod(a, 8) == 3){
        I = 0
        power = as.integer(0)
        while(flag == FALSE){
          if(mod.power(3,power,pk) == a0){flag = TRUE
          } else {power  = power + 1}
        }
      } else {
        power = as.integer(0)
        I = 1
        while(flag == FALSE){
          if(mod.power(-3,power,pk) == a0){flag = TRUE
          } else {power = power + 1}
        }
      }
      result_temp = mod.lin.eq(n, power, pk/(p^2), disp)
      v_temp = c()
      for(k in pk:1){
        if(pk/result_temp[2] == k) break
      }
      for(l in 0:(k-1)){
        v_temp = append(v_temp, result_temp[1] + result_temp[2]*l)
      }
      powered_temp = sapply(v_temp, mod.power, x = 3, n = pk)
      powered_temp = unique(powered_temp)
      if(I == 0){
        for(j in powered_temp){
          print(j)
          powered_temp = append(powered_temp, mod(-1*j, pk))
        }
        powered_temp = unique(powered_temp)
      }
      powered_temp = sort(powered_temp)
      y = append(y, powered_temp)
      y = append(y, '#')
      m_tab = append(m, pk)
      printt('Equation: x^',n,' = ', a0, ' mod ', pk ,'. RESULTS: ', mapply(paste0, powered_temp, ', '), ' mod ', pk)
      Y[i] = result_temp[1]
      M[i] = result_temp[2]
    } else {
      p_root = find.primeroot(pk)
      printq(paste0('Prime root for ', pk, ' is ', p_root))
      printq(paste0("Calculating log{', p_root, '}(',a,') using Shanks baby-step giant-step algorithm"))
      loga = dlog.shanks(a, p_root,pk,disp, FALSE)
      modulus = eulersPhi(pk)
      printq(paste0(n, 'y = ', loga, ' mod ', modulus))
      d = GCD(n, modulus)
      printq(paste0('d := (',n,',', modulus,') = ', d))
      if(!(loga %% d == 0)){
        printq(paste0('Requisite condition is d|log_{g}(a) (log_{g}(a) = ', loga, ', d = ',d, ') which is not satisfied'))
        return(FALSE)
      } else {
        result_temp = mod.lin.eq(n,loga,modulus)
        v_temp = c()
        for(k in pk:1){
          if(modulus/result_temp[2] == k) break
        }
        for(l in 0:(k-1)){
          v_temp = append(v_temp, result_temp[1] + result_temp[2]*l)
        }
        v_temp = sort(unique(v_temp))
        powered_temp = sapply(v_temp, mod.power, x = p_root, n = pk)
        powered_temp = sort(unique(powered_temp))
        y = append(y, powered_temp)
        y = append(y, '#')
        m_tab = append(m_tab, pk)
        printt('Equation: x^',n,' = ', a0, ' mod ', pk ,'. RESULTS: ', mapply(paste0, powered_temp, ', '), ' mod ', pk)
        Y[i] = result_temp[1]
        M[i] = result_temp[2]
      }
    }
  }
  printq('#####################')
  printq('Final results:')
  pointer = 1
  m_tab = m_tab[-1]
  for(i in 1:tab[1]){
    vect = c()
    pointer = pointer + 1
    while(y[pointer] != '#'){
      vect = c(vect, y[pointer])
      pointer = pointer + 1
    }
    if(disp) printt('x = ', mapply(paste0, vect, ', '), ' mod ', m_tab[i])
  }
  result = mod.equations(Y,M,disp)
  if(disp) printt('First result: ', result[1], ' mod ', result[2])
  return(result)
}




#' Jacobi symbol
#'
#' Calculates Jacobi symbol, of course also Legendre's symbol.
#'
#' @param a number on the top
#' @param n number on the bottom
#' @param disp choose whether to display stepwise, def. FALSE
#' @return -1, 0 or 1
#' @export
jacobi.symbol = function(a,n, disp = FALSE){
  if(!all.int(a,n)) stop('Not all of given entries are integers!')
  if(mod(a,n) == n) return(0)
  if(disp) printq = function(x, ...){printt(x,...)}
  if(!disp) printq = function(x, ...){}
  printq('Calculating Jacobi symbol for a = ', a, ' over n  = ', n)
  if(a == 2) return((-1)^((n^2 - 1)/8))
  printq("Using Euler's algorithm")
  if(a < n){
    if(mod((a-1)*(n-1)/4, 2) == 0){A = 0
    } else {A = 1}
    temp = a
    a = n
    n = temp
  } else {
    if(a == 2){A = (-1)^((n^2 - 1)/8)
    } else {A = 0}
  }
  if(a == 2) return((-1)^((n^2 - 1)/8))
  R = c(a, n)
  s = c(0)
  q = c(0)
  i = 2
  while(R[i] != 1){
    temp_s = 0
    tab = euclidean.div(R[i-1], R[i])
    temp_R = tab[2]
    q[i] = tab[1]
    if(temp_R == 1){R[i+1] = 1; s[i] = 1
    } else {
      while(mod(temp_R, 2) == 0){
        temp_s = temp_s + 1
        temp_R = temp_R/2
      }
    }
    s[i] = temp_s
    R[i+1] = temp_R
    printq(R[i-1], ' = ', q[i], '*', R[i], ' + 2^', s[i], '*',R[i+1])
    i = i+1
  }
  R = R[-length(R)]
  for(i in 2:length(R)){
    A = A + s[i]*((R[i]^2 - 1)/8)
    if(i != length(R)) A = A + (R[i]-1)*(R[i+1]-1)/4
  }
  res_p = mod(A,2)
  printq('A = ', A, '.    A = ', res_p, ' mod 2')
  if(res_p == 0) res = 1
  if(res_p == 1) res = -1
  printq('Result: ', res)
  return(res)
}




#' Discrete Square Root
#'
#' Tonell-Shank's algorithm for discrete square root.
#'
#' @param a number to be square rooted
#' @param p modulus
#' @return vector consisting of 2 results
#' @export
sq.root.ts = function(a,p, disp = TRUE){
  if(!all.int(a,p)) stop('Not all of given entries are integers!')
  if(disp) printq = function(x, ...){printt(x,...)}
  if(!disp) printq = function(x, ...){}

  printq("Tonell-Shanks' algorithm for discreet square root")
  printq('x^2 = ',a, ' mod ', p)
  if(jacobi.symbol(a,p) == -1) stop('Solution does not exists!')
  if(mod(p,4) == 3){
    res = mod.power(a,(p+1)/4,p, FALSE)
    printq('For p = 3 mod 4 the result is given as a^((p+1)/4) = ', res)
    return(c(res, mod((-1)*res,p)))
  }
  if(mod(p,8) == 1){
    for(i in 2:(p-1)){
      if(jacobi.symbol(i,p) == -1) break
    }
    d = i
    printq(paste0('We are looking for non-square-root mod ',p, '. We find d = ', d))
    printq('We are looking for factorisation p-1 = 2^r*s, where s is odd')
    temp = p-1
    counter = 0
    while(temp %% 2 == 0){
      counter = counter + 1
      temp = temp/2
    }
    r = counter
    printq('r = ', r)

    s = temp
    printq('s = ', s)

    a_inv = mod.inv(a,p)
    printq('a^-1 = ', a_inv)

    printq('Iterising. Starting entries:')
    x = c(mod(a^((s+1)/2),p))
    x = c(mod.power(a, (s+1)/2, p))
    printq('x0 = ', x[1])

    D = mod.power(d,s, p)
    printq('D = ', D)

    orderus = ord(x[1]^2 * a_inv, p)
    printq('Order of x1^2 * a^-1 in Zp is equal to ', orderus)
    t = c(log2(orderus))
    printq(paste0('t0 = ', t))

    for(i in 1:(r-1)){
      printq('Step ', i)

      printq('x{i+1} = xi * D^(2^(r-ti-1))')
      powerspower = r - t[i] - 1
      printq('r- ti - 1 = ', powerspower)

      POWER = 2^powerspower
      printq('Power of D = ', POWER)

      D_power = D^POWER
      printq('D to power above = ', D_power)
      D_power = mod(D_power, p)
      printq('D mod p = ', D_power)
      x[i+1] = x[i]*D_power
      x[i+1] = mod(x[i+1], p)
      printq('x', i,' = ', x[i+1])

      temp = mod(x[i+1]^2 * a_inv,p)
      printq('Calculating xi^2*a^-1 in Zp. ')
      printq('x', i,'^2 * a^-1 = ', temp)

      temp = ord(temp, p)
      printq('ord_p(*) = ', temp)

      t[i+1] = log2(temp)
      printq('t', i,' = ', t[i+1])

      if(t[i+1] == 0){
        printq('Solution: ', x[i+1])
        printq('2nd solution (= - solution) ', mod(p-x[i+1],p))
        return(c(x[i+1], mod(p-x[i+1],p)))
      }
    }
  }
  if(mod(p,8) == 5){
    printq('mod(p,8) == 5, so checking if a^((p+3)/8) is the solution')
    x0 = mod.power(a, (p+3)/8, p, FALSE)
    if(x0 == a){
      printq('It is')
      return(c(x0, mod((-1)*x0,p)))
    }
    printq("x1 = x0 * 2^((p-1)/4)")
    x1 = mod(x0*mod.power(2, (p-1)/4, p),p)
    return(c(x1, mod((-1)*x1,p)))
  }
}




#' Proth's algorithm for prime
#'
#' Checks whether given number is prime number. Algorithm named after Proth.
#'
#' @param n number
#' @param disp choose whether to see solution stepwise, def. TRUE
#' @return
#' @export
prime.proth = function(n, disp = TRUE){
  if(!is.int(n)) stop('Not proper entries')
  if(disp) printq = function(x, ...){printt(x,...)}
  if(!disp) printq = function(x, ...){}

  printq("Proth's test of being prime for n = ", n)
  printq('Factorise: n-1 = 2^r*s, s - odd')
  temp = n-1
  counter = 0
  while(temp %% 2 == 0){
    counter = counter + 1
    temp = temp/2
  }
  r = counter
  printq('r = ', r)
  s = temp
  printq('s = ', s)
  printq('Checking if 2^r > s: ', 2^r>s)
  for(i in 2:(n-1)){
    if(jacobi.symbol(i,n) == -1) break
  }
  d = i
  printq('Non-square-root of n:  d = ', d)
  printq('Calculating d^((n-1)/2) mod n')
  d_pow = mod.power(d, (n-1)/2, n)
  if(d_pow != mod(-1, n)){
    printq(paste0('d^(*) equals ', d_pow, ', so n is not prime'))
  } else {
    printq(paste0('d^(*) equals ', d_pow, ', ( = -1) so n is prime'))
  }
}




#' Strongly pseudo-prime
#'
#' Check whether the number a is strongly pseudo-prime with respect to n
#'
#' @param a number to be checked
#' @param n modulus
#' @return boolean value
#' @export
is.spsp = function(a,n){
  if(!all.int(n,a)) stop('Not proper entries')
  if(disp) printq = function(x, ...){printt(x,...)}
  if(!disp) printq = function(x, ...){}

  printq("Miller-Rabin algorithm")
  m = n-1
  printq('Factor n-1 as 2^r*s, s - odd')
  r = 0
  while(m %% 2 == 0){
    r = r+1
    m = m/2
  }
  s = m
  printq('r = ', r, '; s = ', s)
  y = c(mod.power(a, s, n))
  printq('y0 = a^s mod n. Calculated y0 = ', y[1])
  if(y[1] == 1){
    printq('n = ', n, ' is strongly pseudo-prime with respect to a = ',a)
    return(TRUE)
  } else {
    for(i in 1:r){
      y[i+1] = mod.power(y[i],2, n)
      printq('y', i, ' = ', y[i+1])
      if(y[i+1] == -1){
        return(TRUE)
      }
    }
  }
  return(FALSE)
}




#' Factorisation rho - 1
#'
#' Pollard rho-1 algorithm for factorisation, requires given entries for calculating.
#'
#' @param n number to be factorised
#' @param B parameter. With respect to B there are searched numbers, algorithm searches for lower prime divisors
#' @param A0 starting point
#' @return number or fiasco
#' @export
factor.po.rho1 = function(n, B, A0){
  if(!all.int(n,B)) stop('Not proper entries')
  if(disp) printq = function(x, ...){printt(x,...)}
  if(!disp) printq = function(x, ...){}

  printq("rho-1 Pollard's factorisation")
  if(!(A0<n & A0 >= 2)) stop('Not valid A0, should be not lesser then 2')

  Primes = function(n){
    res = c(2)
    for(i in 3:n){
      if(is.prime(i)) res = append(res, i)
    }
    return(res)
  }

  primes = Primes(B)
  A = c(A0)
  G = c(GCD(n,A0))
  printq('Primes <= B:')
  printq(mapply(paste0, primes, ' '))
  if(G[1] == 1 | G[1] == n){
    funx = function(t,n){
      temp1 = log(n)
      temp2 = log(t)
      return(floor(temp1/temp2))
    }
    printq('Calculating m, m[i] = floor(ln(n)/ln(pi))')
    m = sapply(primes, funx, n=n)
    printq(mapply(paste, m, ''))
    for(i in 1:length(m)){
      A[i+1] = mod.power(A[i], primes[i]^m[i],n)
    }
    if(disp) print(A[-1])
    printq('calculating G, G[i] = GCD(Ai - 1, n)')
    G = append(G, sapply(A[-1] - 1, GCD, b = n))
    printq('As the result:')
    if(disp) print(G)
    for(i in G){
      if(i != 1) return(i)
    }
  }
}




#' Pollard's rho factorisation
#'
#' Algorithm for factorisation with given parameters. Freely change parameters,
#' it is a probability algorithm so may or may not find the result, change of parameters
#' may allow to get results.
#'
#' @param n number to be checked
#' @param x0 it is iterised with starting point at x0, def. 62
#' @param C there is used function f of x = x^2 + C, def. C = 1
#' @param m number of steps
#' @return number or fiasco
#' @export
factor.po.rho = function(n, x0 = 62, C = 1, m = 18){
  if(!all.int(n,x0,C,m)) stop('Not proper entries')
  if(disp) printq = function(x, ...){printt(x,...)}
  if(!disp) printq = function(x, ...){}

  printq("Rho-Pollard's factorisation algorithm")
  f = function(x){
    return(x^2 + C)
  }
  x = c(x0)
  printq('Interation for x, starting at x0 =', x0)
  for(i in 1:m){
    x[i+1] = mod(f(x[i]),n)
  }
  printq('x = ', mapply(paste0, x, ','))
  printq('Iteration for G ( GCD(x2i - xi, n) )')
  G = c()
  for(i in 1:floor(m/2)){
    G[i] = GCD(x[2*i] - x[i], n)
    if(G[i] != 1 & G[i] != n){
      printq('Xi =', x[i], '    X2i =', x[2*i])
      printq('i =', i, '   Gi =', G[i])
      return(G[i])
    }
  }
  printq(G)
  printq('Search did not yeild any prime divisor')
  return(FALSE)
}




#' Addition on elliptic curve
#'
#' Adds two points on curve C = x^3 + ax mod p
#'
#' @param P,Q points on the curve
#' @param a C = x^3 + ax
#' @param p modulus
#' @param choose whether to see stepwise solution, def. TRUE
#' @return coordinates
#' @export
elliptic.add = function(P, Q, a, p, disp = TRUE){
  if(!all.int(a,p, P[1], P[2], Q[1], Q[2])) stop('Not proper entries')
  if(disp) printz = function(x, ...){printt(x,...)}
  if(!disp) printz = function(x, ...){}

  x1 = P[1]
  x2 = Q[1]
  y1 = P[2]
  y2 = Q[2]

  if(x1 != x2){
    m = mod(y2 - y1, p)
    m = mod(m*mod.inv(mod(x2-x1,p),p),p)
    printz('Calculating m = (y2 - y1)*(x2-x1)^(-1):  m = ', m)
    x3 = mod(m^2 -x1 - x2,p)
    printz('Calculating x3 = m^2 -x1 - x2:   x3 = ', x3)
    y3 = mod(m*(x1 - x3) - y1,p)
    printz('Calculating y3 = m*(x1 - x3) - y1:   y3 = ', y3)
    return(c(x3,y3))
  }
  if(x1 == x2 & y1 != y2){
    return(c(0,0))
  }
  if(x1 == x2 & y1 == y2 & y1 != 0){
    m = mod(3*x1^2 + a,p)
    m = mod(m*mod.inv(2*y1,p),p)
    printz('Calculating m = (3*x1^2 + a)/y1:   m = ', m)
    x3 = mod(m^2 - 2*x1,p)
    printz('Calculating x3 = m^2 - 2*x1:   x3 = ', x3)
    y3 = mod(m*(x1 - x3) - y1,p)
    printz('Calculating y3 = m*(x1 - x3) - y1:   y3 = ', y3)
    return(c(x3,y3))
  }
  if(x1 == x2 & y1 == y2 & y1 == 0){
    return(c(0,0))
  }
}



#' Multiply point on the curve
#'
#' Allows to count n*P on the elliptic curve C = x^3 + ax modulo p
#' Not recommended for large numbers, check then elliptic.mutiplicity.a
#'
#' @param P point
#' @param k power
#' @param a C = x^3 + ax
#' @param p modulus
#' @param disp choose whether to see stepwise solution
#' @return coordinates
#' @export
elliptic.multiplicity = function(P,k,a,p){
  if(!all.int(a,p, P[1], P[2], k)) stop('Not proper entries')

  temp = P
  if(k == 1) return(P)
  for(i in 2:k){
    temp = elliptic.add(temp, P, a, p, FALSE)
  }
  return(temp)
}




#' Multiply point on the curve
#'
#' Allows to count n*P on the elliptic curve C = x^3 + ax modulo p
#' Different from elliptic.multiplicity, because uses so-called
#' fast powering algorithm. Recommended for large numbers
#'
#' @param P point
#' @param k power
#' @param a C = x^3 + ax
#' @param p modulus
#' @param disp choose whether to see stepwise solution
#' @return coordinates
#' @export
elliptic.multiplicity.a = function(P,k,a,p, disp = TRUE){
  if(!all.int(a,p, P[1], P[2], k)) stop('Not proper entries')
  if(disp) printq = function(x, ...){printt(x,...)}
  if(!disp) printq = function(x, ...){}

  if(k==1) return(x)
  printq('Algorytm szybkiego mnozenie dla eliptycznych')
  j = k
  pots = c()
  while(j != 1 & j != 0){
    if(j %% 2 == 0){
      new_j = j/2
      printq(paste0(j, ' = 2 * ', new_j))
      j = new_j
      pots = append(pots, c(1,0))
    } else{
      new_j = (j-1)/2
      printq(paste0(j, ' = 2 * ', new_j, ' + 1'))
      j = new_j
      pots = append(pots, c(1,1))
    }
  }
  t = 1
  P_temp = P
  for(i in (length(pots)/2):1){
    t = 2*t
    P_temp = elliptic.multiplicity(P_temp,2,a,p)
    printq(paste0('P*', t, ' = ', P_temp[1],' ', P_temp[2]))
    if(pots[2*i] == 1){
      t = t + 1
      P_temp = elliptic.add(P_temp, P, a, p, FALSE)
      printq(paste0('P*', t, ' = ', P_temp[1],' ',P_temp[2]))
    }

  }
  return(P_temp)
}




#' Order of point on elliptic curve
#'
#' Check the order of the point on the elliptic cruve C = x^3 + ax.
#'
#' @param P point
#' @param a C = x^3 + ax
#' @param p modulus
#' @return number
#' @export
elliptic.ord = function(P, a, p){
  for(i in 1:(10*p)){
    if(elliptic.multiplicity(P,i,a,p)[1] == 0 & elliptic.multiplicity(P,i,a,p)[2] == 0) return(i)
  }
}




#' Discrete logarithm on curve
#'
#' Calculates discrete logarithm on the elliptic curve using
#' Shanks's baby-step giant-step algorithm. Looks for the solution
#' of equation n*P = Q mod Zp with respect to n
#'
#' @param P starting point, the base
#' @param Q finishing point, the power
#' @param a C = x^3 + ax
#' @param p modulus
#' @param disp choose whether to display stepwise solution
#' @return number
#' @export
elliptic.dlog.sh = function(P, Q, a, p, disp = TRUE){
  if(!all.int(a,p, P[1], P[2], Q[1], Q[2])) stop('Not proper entries')
  if(disp) printq = function(x, ...){printt(x,...)}
  if(!disp) printq = function(x, ...){}

  printq('Algorithm Shanks baby-giant steps for elliptic curves')
  printq('Entries for equation n*P = Q mod Zp on curve E = x^3 + ax + b')
  printq('x1 =', P[1],', y1 =', P[2], ', x2 =', Q[1],'y2 =', Q[2], ', p =', p, ', a =', a)
  m = ceiling(sqrt(p)) + 1
  printq('m = ceiling(sqrt(p)) + 1 = ', m)
  P_inv = elliptic.multiplicity(P,m, a, p)
  printq('m*P =', P_inv[1], P_inv[2])
  PP = c(P_inv[1], (-1)*P_inv[2])
  printq('-mP = ', PP[1], PP[2])
  printq('Creating the list of baby-giant steps')
  printq('baby steps = 0, P*1,..,P*n, giant steps: Q,Q+P*(-n),Q+P*(-2n),...,Q+P*(-n^2)')
  baby_steps = c(0,0)
  giant_steps = Q
  for(i in 1:m){
    print(i)
    if(i == 1){
      baby_steps = rbind(baby_steps, P)
      giant_steps = rbind(giant_steps, elliptic.add(giant_steps, PP, a, p, FALSE))
    } else {
      baby_steps = rbind(baby_steps, elliptic.add(baby_steps[i,], P, a, p, FALSE))
      giant_steps = rbind(giant_steps, elliptic.add(Q, elliptic.multiplicity(PP,i, a, p), a, p, FALSE))
    }
  }
  dfm = data.frame(t(baby_steps))
  colnames(dfm) = mapply(paste0, 'k=', 0:m)
  dfw = data.frame(t(giant_steps))
  colnames(dfw) = mapply(paste0, 'k=', 0:m)
  df = rbind(dfm, dfw)
  rownames(df) = c('kP[x]', 'kP[y]', 'Q + k(-MP) [x]', 'Q + k(-MP) [y]')
  if(disp) print(df)
  printq('Looking for common entry for both lists')
  for(i in 1:(m+1)){
    stop = FALSE
    for(j in 1:(m+1)){
      if(df[1,i]==df[3,j]){
        if(df[2,i] == df[4,j])
          printq('Found. It is for k = ', i-1, ' in the first list and for k = ',j-1, ' in the second')
        temp = c(i-1,j-1)
        stop = TRUE
        break
      }
    }
    if(stop){break}
  }
  i = temp[1]
  j = temp[2]
  printq('i*P = Q + (-mP)*j, where i is value of k for the first(2nd) row and j - value of k for the 3rd(4th)')
  printq('n =', i, '+',j,'*',m)
  x = i+j*m
  printq('Solution: n = ', x , 'mod', p)
  return(x)
}
