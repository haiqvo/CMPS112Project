package troop;

public class NullSworderCommander extends SworderCommander implements Commander
{
    public static final NullSworderCommander singleton = new NullSworderCommander();
    protected int str_inc = 0;
    
    private NullSworderCommander()
    {
        super(0, 4);
    }
    
    @Override
    public int getStrength()
    {
        return 0;
    }
    
    public String toString()
    {
        return "None";
    }

    @Override
    public void upgradeCommand()
    {
        // do nothing
    }

    @Override
    public int getCommand()
    {
        return 4;
    }
}
