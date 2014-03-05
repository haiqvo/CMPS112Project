package troop;

public class SworderCommander extends Sworder implements Commander
{
    protected int command = 5;
    
    public SworderCommander()
    {
        super();
    }
    
    public SworderCommander(int strength, int command)
    {
        super(strength);
        this.command = command;
    }
    
    @Override
    public void upgradeCommand()
    {
        upgrades++;
        command++;
    }
    
    @Override
    public int getCommand()
    {
        return command;
    }
    
    @Override
    public String toString()
    {
        return "("+command+", "+strength+")";
    }
}
