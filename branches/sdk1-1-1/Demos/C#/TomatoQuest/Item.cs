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

namespace TomatoQuest
{
    public class Item
    {
        public Sprite Sprite;
        public bool Dropped;

        //Item Constructor
        public Item(String name)
        {
            //Load the Sprite Image
            Sprite = Graphics.CreateSprite(Resources.GameImage(name));

            //Do not use Pixel level collision
            Sprite.UsePixelCollision = false;

            //Whether the item is on the ground, default false
            Dropped = false;
        }

        public bool CharacterCollidedWithItem(Character theCharacter)
        {
            //Check if the Character is alive and the item is on the ground
            if (theCharacter.Alive && Dropped)
            {
                //Check if the Character and Item collide
                if (Physics.HaveSpritesCollided(theCharacter.Sprite, Sprite))
                {
                    return true;
                }
            }

            return false;
        }

        public static void GiveCharacterItem(Character theCharacter, Item theItem)
        {
            //Add the Item to the Characters Inventory
            theCharacter.Item = theItem;

            //Set the Items dropped state to false
            theItem.Dropped = false;
        }

        public void DrawItem()
        {
            //If the Item is dropped, draw the item
            if (Dropped)
            {
                Graphics.DrawSprite(Sprite);
            }
        }

        public void DropItem()
        {
            //Sets the Dropped Value of the item to true
            Dropped = true;
        }
    }
}
