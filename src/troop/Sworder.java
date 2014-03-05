package troop;

public class Sworder extends AbstractTroop
{
    protected int str_inc = 1000;
    
    public Sworder()
    {
        strength = str_inc;
    }

    public Sworder(int strength)
    {
        this.strength = strength;
    }
}
