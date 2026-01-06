let x = 10;
{
    let y = 20;
    x = x + y; 
}
return y;  // Eroare: y nu este definit în acest scope
return x;