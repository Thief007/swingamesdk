using System;
using System.Drawing;
using System.Collections.Generic;

using SwinGame;
using Graphics = SwinGame.Graphics;
using Bitmap = SwinGame.Bitmap;
using Font = SwinGame.Font;
using FontStyle = SwinGame.FontStyle;
using Event = SwinGame.Event;
using CollisionSide = SwinGame.CollisionSide;
using Sprite = SwinGame.Sprite;

using GameResources;

namespace GameProject
{
    public class Item
    {
        public Sprite Sprite;
        public bool Dropped;
    }

    public static class Items
    {
        public static Item NewItem(String name)
        {
            Item temp = new Item();

            //Load the Sprite Image
            temp.Sprite = Graphics.CreateSprite(Resources.GameImage("Tomato"));

            //Do not use Pixel level collision
            temp.Sprite.UsePixelCollision = false;

            //Whether the item is on the ground, default false
            temp.Dropped = false;

            return temp;
        }

        public static bool CharacterCollideWithItem(Character theCharacter, Item theItem)
        {
            //Check if the Character is alive and the item is on the ground
            if (theCharacter.Alive && theItem.Dropped)
            {
                //Check if the Character and Item collide
                if (Physics.HaveSpritesCollided(theCharacter.Sprite, theItem.Sprite))
                {
                    return true;
                }
            }

            return false;
        }

        public static void GiveCharacterItem(ref Character theCharacter, ref Item theItem)
        {
            //Give the item to the character
            theCharacter.Item = theItem;
            //theCharacter.Item.Dropped = false;
            theItem.Dropped = false;

            theCharacter.HasItem = true;
        }

        public static void DropItem(ref Character theCharacter)
        {
            //Position the Item where the Character is
            
          
        }

        public static void DrawItem(Item item)
        {
            if (item.Dropped)
            {
                Graphics.DrawSprite(item.Sprite);
            }
        }
    }
}
