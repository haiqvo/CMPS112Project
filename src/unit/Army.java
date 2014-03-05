package unit;

import java.util.ArrayList;

public class Army
{
    public ArrayList<SworderSquad> sworders = new ArrayList<SworderSquad>();
    public ArrayList<ArrowerSquad> arrowers = new ArrayList<ArrowerSquad>();
    public ArrayList<ScatteredSquad> scattered = new ArrayList<ScatteredSquad>();
        
    public Army()
    {
    }
    
    public Army(ArrayList<SworderSquad> sworders, ArrayList<ArrowerSquad> arrowers, ArrayList<ScatteredSquad> scattered)
    {
        this.sworders = sworders;
        this.arrowers = arrowers;
        this.scattered = scattered;
    }
    
    public void print()
    {
        System.out.println("Sworders:");
        for(SworderSquad s : sworders)
        {
            s.print();
        }
        
        System.out.println("Arrowers:");
        for(ArrowerSquad s : arrowers)
        {
            s.print();
        }
        
        System.out.println("Scattered:");
        for(ScatteredSquad s : scattered)
        {
            s.print();
        }
    }
}
