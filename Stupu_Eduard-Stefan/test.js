let global = 100;
let modificata = 10;
let shadowed = "original";

{
    let local_exterior = 50;
    
    modificata = modificata + local_exterior;
    
    let shadowed = "in bloc";
    
    {
        let local_interior = 25;
        
        global = global + local_interior;
    }
}